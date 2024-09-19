package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.IProcess;
import com.midorum.win32api.facade.IWindow;
import com.midorum.win32api.facade.Rectangle;
import com.midorum.win32api.facade.Win32System;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.common.FileServiceProvider;
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

    public static final int DEVIATION = 1;
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
                            return Collections.EMPTY_LIST;
                        }).stream().findFirst());
    }

    public Optional<IWindow> getWindow(final String windowTitle, final String windowClassName) {
        return windowCache.computeIfAbsent(windowTitle + "_class_" + windowClassName,
                key -> Win32System.getInstance()
                        .findAllWindows(windowTitle, windowClassName, true)
                        .stream().findFirst());
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
                        final StampSeeker stampSeeker = new StampSeeker(screenImage, stamp, DEVIATION);
                        final List<StampSeeker.Result> stampSeekResults = stampSeeker.perform();
                        if(stampSeekResults.isEmpty()) return Optional.empty();
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
