package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.IProcess;
import com.midorum.win32api.facade.IWindow;
import com.midorum.win32api.facade.Rectangle;
import com.midorum.win32api.facade.exception.Win32ApiException;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.common.FileServiceProvider;
import midorum.win32.deputy.common.GuardedWin32Adapter;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Settings;
import midorum.win32.deputy.model.Stamp;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

public class Win32Cache {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final File workingDirectory;
    private final GuardedWin32Adapter win32Adapter;
    private final Settings settings;

    private final ConcurrentHashMap<String, Optional<IProcess>> processCache = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Optional<IWindow>> windowCache = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Optional<Stamp>> stampCache = new ConcurrentHashMap<>();

    public Win32Cache(final File workingDirectory, final GuardedWin32Adapter win32Adapter, final Settings settings) {
        this.workingDirectory = workingDirectory;
        this.win32Adapter = win32Adapter;
        this.settings = settings;
    }

    public void invalidate() {
        processCache.clear();
        windowCache.clear();
        stampCache.clear();
        logger.debug("cache invalidated");
    }

    public Optional<IProcess> getProcess(final String processName) {
        logger.debug("getProcess: processName:{}", processName);
        return processCache.computeIfAbsent(processName,
                key -> win32Adapter.listProcessesWithName(key)
                        .getOrHandleError(e -> {
                            logger.error("error occurred while obtaining system processes", e);
                            return List.of();
                        }).stream()
                        .peek(process -> logger.info("found process \"{}\" ({})", process.name(), process.pid()))
                        .findFirst());
    }

    public Optional<IProcess> getProcess(final String processName,
                                         final Long existsGreaterThan,
                                         final Long existsLessThan) {
        logger.debug("getProcess: processName:{} existsGreaterThan:{} existsLessThan:{}",
                processName, existsGreaterThan, existsLessThan);
        final String key = processName + ">" + existsGreaterThan + "<" + existsLessThan;
        return processCache.computeIfAbsent(key,
                _ -> win32Adapter.listProcessesWithName(processName)
                        .getOrHandleError(e -> {
                            logger.error("error occurred while obtaining system processes", e);
                            return List.of();
                        }).stream()
                        .filter(process -> {
                            final Long creationTime = process.getCreationTime().getOrHandleError(e -> {
                                logger.error(() -> "error occurred while obtaining process \"" + processName
                                        + "\" attributes", e);
                                return null;
                            });
                            if (creationTime == null) return false;
                            final long now = System.currentTimeMillis();
                            if (existsGreaterThan != null
                                    && creationTime + TimeUnit.MILLISECONDS.convert(existsGreaterThan, TimeUnit.SECONDS) < now)
                                return true;
                            return existsLessThan != null
                                    && creationTime + TimeUnit.MICROSECONDS.convert(existsLessThan, TimeUnit.SECONDS) > now;
                        })
                        .peek(process -> logger.info("found process \"{}\" ({})", process.name(), process.pid()))
                        .findFirst());
    }

    public Optional<IWindow> getWindow(final String windowTitle, final String windowClassName) {
        logger.debug("getWindow: windowTitle:{} windowClassName:{}", windowTitle, windowClassName);
        return windowCache.computeIfAbsent(windowTitle + "_class_" + windowClassName,
                key -> {
                    logger.debug("getWindow: key \"{}\" not found - try get data", key);
                    final Optional<IWindow> found = win32Adapter.findAllWindows(windowTitle, windowClassName)
                            .stream()
                            .peek(window -> logger.info("found window \"{}\" ({})", window.getText(), window.getSystemId()))
                            .findFirst();
                    if (settings.listAllWindowsWhenSearchFail() && found.isEmpty()) {
                        logger.debug("Window {} class:{} not found. List all windows in system.", windowTitle, windowClassName);
                        win32Adapter.listAllWindows().forEach(window ->
                                logger.debug("found window: id{} title:{} class:{}",
                                        window.getSystemId(), window.getText(), window.getClassName()));
                    }
                    return found;
                });
    }

    public Optional<Stamp> getStamp(final String stampPath, final boolean doNotTouchMousePointer) {
        logger.debug("getStamp: stampPath:{}", stampPath);
        return stampCache.computeIfAbsent(stampPath,
                key -> {
                    logger.debug("getStamp: key \"{}\" not found - try get data", key);
                    try {
                        final BufferedImage stampImage = new FileServiceProvider().withFile(
                                CommonUtil.getPathForImages(workingDirectory),
                                key,
                                CommonUtil.getImageFormat()).readImage();
                        final Stamp stamp = new Stamp(key, stampImage);
                        final List<StampSeeker.Result> stampSeekResults = seekStampOnScreenWithAdditionalAttempt(stamp, doNotTouchMousePointer);
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
                    } catch (IOException | InterruptedException | Win32ApiException e) {
                        logger.error("error occurred while reading image file", e);
                        throw new IllegalStateException(e);
                    }
                });
    }

    private List<StampSeeker.Result> seekStampOnScreenWithAdditionalAttempt(final Stamp stamp, final boolean doNotTouchMousePointer)
            throws InterruptedException, Win32ApiException {
        final List<StampSeeker.Result> stampSeekResults = seekStampOnScreen(stamp);
        if (doNotTouchMousePointer || !stampSeekResults.isEmpty()) return stampSeekResults;
        // maybe the mouse pointer interferes
        win32Adapter.getScreenMouse().move(0, 0);
        return seekStampOnScreen(stamp);
    }

    private List<StampSeeker.Result> seekStampOnScreen(final Stamp stamp) {
        logger.trace("seekStampOnScreen: {}", stamp.name());
        final BufferedImage screenImage = win32Adapter.takeScreenShot();
        final StampSeeker stampSeeker = new StampSeeker(screenImage, stamp, settings.stampDeviation());
        return stampSeeker.perform();
    }
}
