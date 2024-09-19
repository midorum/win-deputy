package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.*;
import com.midorum.win32api.facade.exception.Win32ApiException;
import com.midorum.win32api.struct.PointInt;
import com.midorum.win32api.win32.MsLcid;
import com.midorum.win32api.win32.Win32VirtualKey;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;

public class CommandProcessor {

    public static final int MOUSE_SPEED_FACTOR = 1;
    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);

    private final Win32Cache cache;
    private final Win32System win32System;

    public CommandProcessor(final Win32Cache cache) {
        this.cache = cache;
        this.win32System = Win32System.getInstance();
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
        }
    }

    private void minimizeAllWindows() {
        win32System.minimizeAllWindows();
    }

    private void mouseLeftClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32System.getScreenMouse(MOUSE_SPEED_FACTOR);
        mouse.move(getMousePosition(data));
        mouse.leftClick();
    }

    private void mouseDoubleLeftClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32System.getScreenMouse(MOUSE_SPEED_FACTOR);
        mouse.move(getMousePosition(data));
        mouse.leftClick().leftClick();
    }

    private void mouseRightClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32System.getScreenMouse(MOUSE_SPEED_FACTOR);
        mouse.move(getMousePosition(data));
        mouse.rightClick();
    }

    private void mouseDoubleRightClick(final Map<CommandDataType, String> data) throws Win32ApiException, InterruptedException {
        final IMouse mouse = win32System.getScreenMouse(MOUSE_SPEED_FACTOR);
        mouse.move(getMousePosition(data));
        mouse.rightClick().rightClick();
    }

    private PointInt getMousePosition(final Map<CommandDataType, String> data) {
        final String mousePositionData = data.get(CommandDataType.mousePosition);
        if (mousePositionData != null) return CommonUtil.stringToPoint(mousePositionData)
                .orElseThrow(() -> new IllegalStateException("cannot parse coordinates from " + mousePositionData));
        final String mouseShotRelatedPositionData = data.get(CommandDataType.mouseShotRelatedPosition);
        if (mouseShotRelatedPositionData != null) {
            final Rectangle location = cache.getStamp(mouseShotRelatedPositionData)
                    .orElseThrow(() -> new IllegalStateException("cannot obtain shot from " + mouseShotRelatedPositionData))
                    .location();
            return new PointInt(location.left(), location.top());
        }
        throw new UnsupportedOperationException("unsupported command data types: " + data);
    }

    private void keyboardType(final Map<CommandDataType, String> data) throws Win32ApiException {
        final String keyboardTypeTextData = data.get(CommandDataType.keyboardTypeText);
        if (keyboardTypeTextData == null) throw new IllegalStateException("cannot obtain keyboardTypeText data");
        final IWindow window = getForegroundWindow();
        final String keyboardLayoutData = data.get(CommandDataType.keyboardLayout);
        if (keyboardLayoutData != null) {
            window.setKeyboardLayout(MsLcid.fromLayoutName(keyboardLayoutData)
                    .orElseThrow(() -> new IllegalStateException("cannot parse keyboard layout: " + keyboardLayoutData)));
        }
        window.getKeyboard().type(keyboardTypeTextData);
    }

    private void keyboardHitKey(final Map<CommandDataType, String> data) throws Win32ApiException {
        final String keyboardKeyCodeData = data.get(CommandDataType.keyboardKeyCode);
        if (keyboardKeyCodeData == null) throw new IllegalStateException("cannot obtain keyboardKeyCode data");
        final IWindow window = getForegroundWindow();
        window.getKeyboard().enterHotKey(new HotKey.Builder()
                .code(Win32VirtualKey.valueOf(keyboardKeyCodeData))
                .setAlt(Boolean.parseBoolean(data.get(CommandDataType.keyboardAltFlag)))
                .setControl(Boolean.parseBoolean(data.get(CommandDataType.keyboardCtrlFlag)))
                .setShift(Boolean.parseBoolean(data.get(CommandDataType.keyboardShiftFlag)))
                .build());
    }

    private IWindow getForegroundWindow() {
        return win32System.getForegroundWindow()
                .orElseThrow(() -> new IllegalStateException("cannot obtain foreground window"));
    }

}
