package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.IProcess;
import com.midorum.win32api.facade.IWindow;
import com.midorum.win32api.facade.Rectangle;
import com.midorum.win32api.facade.Win32System;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.common.FileServiceProvider;
import midorum.win32.deputy.common.Settings;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Stamp;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

public class Win32Cache {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final File workingDirectory;

    private final ConcurrentHashMap<String, Optional<IProcess>> processCache = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Optional<IWindow>> windowCache = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Optional<Stamp>> stampCache = new ConcurrentHashMap<>();

    public Win32Cache(final File workingDirectory) {
        this.workingDirectory = workingDirectory;
    }

    public void invalidate() {
        processCache.clear();
        windowCache.clear();
        stampCache.clear();
    }

    @SuppressWarnings("unchecked")
    public Optional<IProcess> getProcess(final String processName) {
        return processCache.computeIfAbsent(processName,
                key -> Win32System.getInstance()
                        .listProcessesWithName(key)
                        .getOrHandleError(e -> {
                            logger.error("error occurred while obtaining system processes", e);
                            return (List<IProcess>) Collections.EMPTY_LIST;
                        }).stream()
                        .peek(process -> logger.info("found process \"{}\" ({})", process.name(), process.pid()))
                        .findFirst());
    }

    public Optional<IWindow> getWindow(final String windowTitle, final String windowClassName) {
        return windowCache.computeIfAbsent(windowTitle + "_class_" + windowClassName,
                key -> {
                    final Optional<IWindow> found = Win32System.getInstance()
                            .findAllWindows(windowTitle, windowClassName, true)
                            .stream()
                            .peek(window -> logger.info("found window \"{}\" ({})", window.getText(), window.getSystemId()))
                            .findFirst();
                    if(Settings.LIST_ALL_WINDOWS_WHEN_SEARCH_FAIL && found.isEmpty()) {
                        logger.debug("Window {} class:{} not found. List all windows in system.", windowTitle, windowClassName);
                        Win32System.getInstance().listAllWindows().forEach(window -> {
                            logger.debug("found window: id{} title:{} class:{}", window.getSystemId(), window.getText(), window.getClassName());
                        });
                    }
                    return found;
                });
    }

    public Optional<Stamp> getStamp(final String stampPath) {
        return stampCache.computeIfAbsent(stampPath,
                key -> {
                    try {
                        final BufferedImage stampImage = new FileServiceProvider().withFile(
                                CommonUtil.getPathForImages(workingDirectory),
                                key,
                                CommonUtil.getImageFormat()).readImage();
                        final Stamp stamp = new Stamp(key, stampImage);
                        final BufferedImage screenImage = Win32System.getInstance().getScreenShotMaker().takeWholeScreen();
                        final StampSeeker stampSeeker = new StampSeeker(screenImage, stamp, Settings.STAMP_DEVIATION);
                        final List<StampSeeker.Result> stampSeekResults = stampSeeker.perform();
                        if (stampSeekResults.isEmpty()) return Optional.empty();
                        return stampSeekResults.stream()
                                .map(result -> {
                                    logger.debug("found stamp: {}", result);
                                    return new Stamp(result.stamp().name(),
                                            result.stamp().wholeData(),
                                            new Rectangle(result.x(), result.y(),
                                                    result.stamp().location().width(),
                                                    result.stamp().location().height()));
                                }).findFirst();
                    } catch (IOException e) {
                        logger.error("error occurred while reading image file", e);
                        throw new IllegalStateException(e);
                    }
                });
    }
}
