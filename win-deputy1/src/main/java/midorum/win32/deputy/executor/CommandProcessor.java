package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.*;
import com.midorum.win32api.facade.exception.Win32ApiException;
import com.midorum.win32api.struct.PointInt;
import com.midorum.win32api.win32.MsLcid;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.common.GuardedWin32Adapter;
import midorum.win32.deputy.common.UserActivityObserver;
import midorum.win32.deputy.common.Win32Adapter;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;
import java.util.Optional;

public class CommandProcessor {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);

    private final Win32Cache cache;
    private final UserActivityObserver userActivityObserver;
    private final GuardedWin32Adapter win32Adapter;

    public CommandProcessor(final Win32Cache cache,
                            final UserActivityObserver userActivityObserver,
                            final GuardedWin32Adapter win32Adapter) {
        this.cache = cache;
        this.userActivityObserver = userActivityObserver;
        this.win32Adapter = win32Adapter;
    }

    public void performCommand(final Command command) throws Win32ApiException, InterruptedException {
        final CommandType commandType = command.getType();
        switch (commandType) {
            case minimizeAllWindows -> minimizeAllWindows();
            case mouseLeftClick -> mouseLeftClick(command.getData());
            case mouseDoubleLeftClick -> mouseDoubleLeftClick(command.getData());
            case mouseRightClick -> mouseRightClick(command.getData());
            case mouseDoubleRightClick -> mouseDoubleRightClick(command.getData());
            case keyboardType -> keyboardType(command.getData());
            case keyboardHitKey -> keyboardHitKey(command.getData());
            case killProcess -> killProcess(command.getData());
        }
    }

    private void minimizeAllWindows() {
        win32Adapter.minimizeAllWindows();
    }

    private void mouseLeftClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32Adapter.getScreenMouse();
        mouse.move(getMousePosition(data));
        mouse.leftClick();
    }

    private void mouseDoubleLeftClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32Adapter.getScreenMouse();
        mouse.move(getMousePosition(data));
        mouse.leftClick().leftClick();
    }

    private void mouseRightClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32Adapter.getScreenMouse();
        mouse.move(getMousePosition(data));
        mouse.rightClick();
    }

    private void mouseDoubleRightClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32Adapter.getScreenMouse();
        mouse.move(getMousePosition(data));
        mouse.rightClick().rightClick();
    }

    private PointInt getMousePosition(final Map<CommandDataType, String> data) {
        final String mousePositionData = data.get(CommandDataType.mousePosition);
        if (mousePositionData != null) return CommonUtil.stringToPoint(mousePositionData)
                .orElseThrow(() -> new IllegalStateException("cannot parse coordinates from " + mousePositionData));
        final String mouseShotRelatedPositionData = data.get(CommandDataType.mouseShotRelatedPosition);
        if (mouseShotRelatedPositionData != null) {
            //FIXME do we need support subtle commands?
            final Rectangle location = cache.getStamp(mouseShotRelatedPositionData, false)
                    .orElseThrow(() -> new IllegalStateException("cannot obtain shot from " + mouseShotRelatedPositionData))
                    .location();
            final String mouseRelativePositionData = data.get(CommandDataType.mouseRelativePosition);
            if (mouseRelativePositionData == null) {
                return new PointInt(location.left(), location.top());
            }
            final Optional<PointInt> pointInt = CommonUtil.stringToPoint(mouseRelativePositionData);
            if (pointInt.isPresent()) {
                final PointInt offset = pointInt.get();
                return new PointInt(location.left() + offset.x(), location.top() + offset.y());
            }
        }
        throw new UnsupportedOperationException("unsupported command data types: " + data);
    }

    private void keyboardType(final Map<CommandDataType, String> data) throws Win32ApiException {
        final String keyboardTypeTextData = data.get(CommandDataType.keyboardTypeText);
        if (keyboardTypeTextData == null) throw new IllegalStateException("cannot obtain keyboardTypeText data");
        final IWindow window = getWindow(data.get(CommandDataType.windowTitle), data.get(CommandDataType.windowClassName));
        final String keyboardLayoutData = data.get(CommandDataType.keyboardLayout);
        if (keyboardLayoutData != null) {
            window.setKeyboardLayout(MsLcid.fromLayoutName(keyboardLayoutData)
                    .orElseThrow(() -> new IllegalStateException("cannot parse keyboard layout: " + keyboardLayoutData)));
        }
        window.getKeyboard().type(keyboardTypeTextData, (order, virtualCode) -> {
            logger.trace("put virtual code to observer: order:{} virtualCode:{}", order, virtualCode);
            final long lastUserKeyEventTime = userActivityObserver.putSelfKeyCode(virtualCode);
            if (lastUserKeyEventTime > 0) throw new UserActionDetectedException();
        });
    }

    private void keyboardHitKey(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final String keyboardKeyStrokeData = data.get(CommandDataType.keyboardKeyStroke);
        if (keyboardKeyStrokeData == null) throw new IllegalStateException("cannot obtain keyboardKeyStroke data");
        final String keyboardKeyStrokeDelayValue = data.get(CommandDataType.keyboardKeyStrokeDelay);
        final long delay = keyboardKeyStrokeDelayValue != null ? Long.parseLong(keyboardKeyStrokeDelayValue) : 0L;
        final HotKey hotKey = HotKey.valueOf(keyboardKeyStrokeData);
        final IWindow window = getWindow(data.get(CommandDataType.windowTitle), data.get(CommandDataType.windowClassName));
        window.getKeyboard().enterHotKey(hotKey, delay, (order, virtualCode) -> {
            logger.trace("put virtual code to observer: order:{} virtualCode:{}", order, virtualCode);
            final long lastUserKeyEventTime = userActivityObserver.putSelfKeyCode(virtualCode);
            if (lastUserKeyEventTime > 0) throw new UserActionDetectedException();
        });
    }

    private void killProcess(final Map<CommandDataType, String> data) throws Win32ApiException {
        if (killProcessByName(data) || killProcessByWindow(data)) {
            logger.info("process has been killed");
        } else {
            logger.info("process has not been killed");
        }
    }

    private boolean killProcessByName(final Map<CommandDataType, String> data) throws Win32ApiException {
        final String processNameData = data.get(CommandDataType.processName);
        if (processNameData == null) return false;
        final String processExistsGreaterThanData = data.get(CommandDataType.processExistsGreaterThan);
        final String processExistsLessThanData = data.get(CommandDataType.processExistsLessThan);
        final Optional<IProcess> process = getProcessFromCache(processNameData, processExistsGreaterThanData, processExistsLessThanData);
        if (process.isEmpty()) return false;
        final IProcess iProcess = process.get();
        logger.info("terminating process \"{}\"", iProcess.name());
        iProcess.terminate();
        return true;
    }

    private boolean killProcessByWindow(final Map<CommandDataType, String> data) throws Win32ApiException {
        final String windowTitleData = data.get(CommandDataType.windowTitle);
        if (windowTitleData == null) return false;
        final String windowClassNameData = data.get(CommandDataType.windowClassName);
        final Optional<IWindow> window = cache.getWindow(windowTitleData, windowClassNameData);
        if (window.isEmpty()) return false;
        final Optional<Either<IProcess>> maybeProcess = window.map(IWindow::getProcess);
        if (maybeProcess.isEmpty()) return false;
        final IProcess process = maybeProcess.get().getOrHandleError(e -> {
            logger.error(() -> "cannot obtain process for window with title \""
                    + windowTitleData + "\" and class \""
                    + windowClassNameData + "\"", e);
            return null;
        });
        if (process == null) return false;
        final String processExistsGreaterThanData = data.get(CommandDataType.processExistsGreaterThan);
        final String processExistsLessThanData = data.get(CommandDataType.processExistsLessThan);
        if (processExistsGreaterThanData != null || processExistsLessThanData != null) {
            final Long creationTime = process.getCreationTime().getOrHandleError(e -> {
                logger.error(() -> "error occurred while obtaining process \"" + process.name()
                        + "\" attributes", e);
                return null;
            });
            if (creationTime == null) return false;
            final long now = System.currentTimeMillis();
            if (processExistsGreaterThanData != null
                    && creationTime + Long.parseLong(processExistsGreaterThanData) > now)
                return false;
            if (processExistsLessThanData != null
                    && creationTime + Long.parseLong(processExistsLessThanData) < now)
                return false;
        }
        logger.info("terminating process \"{}\" by window \"{}\"",
                process.name(), windowTitleData);
        process.terminate();
        return true;
    }

    private Optional<IProcess> getProcessFromCache(final String processNameData,
                                                   final String processExistsGreaterThanData,
                                                   final String processExistsLessThanData) {
        if (processExistsGreaterThanData == null && processExistsLessThanData == null)
            return cache.getProcess(processNameData);
        return cache.getProcess(processNameData,
                processExistsGreaterThanData != null ? Long.valueOf(processExistsGreaterThanData) : null,
                processExistsLessThanData != null ? Long.valueOf(processExistsLessThanData) : null);
    }

    private IWindow getWindow(final String windowTitleData, final String windowClassNameData) {
        return windowTitleData != null || windowClassNameData != null
                ? cache.getWindow(windowTitleData, windowClassNameData)
                .orElseThrow(() -> new IllegalStateException("cannot obtain window with title [" + windowTitleData + "]" +
                        " and class [" + windowClassNameData + "]"))
                : win32Adapter.getForegroundWindow()
                .orElseThrow(() -> new IllegalStateException("cannot obtain foreground window"));
    }

}
