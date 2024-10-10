package midorum.win32.deputy.common;

import com.midorum.win32api.facade.*;
import com.midorum.win32api.facade.Either;
import com.midorum.win32api.struct.PointInt;
import com.midorum.win32api.win32.MsLcid;

import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Optional;

public class Win32Adapter {

    private final Win32System win32System;

    public Win32Adapter() {
        this.win32System = Win32System.getInstance();
    }

    public BufferedImage takeScreenShot() {
        return win32System.getScreenShotMaker().takeWholeScreen();
    }

    public BufferedImage takeRectangleShot(final Rectangle rectangle) {
        return win32System.getScreenShotMaker().takeRectangle(rectangle);
    }

    public Either<MsLcid[]> getAvailableKeyboardLayouts() {
        return win32System.getAvailableKeyboardLayouts();
    }

    public Optional<WindowPoint> getWindowByPoint(final PointInt point) {
        return win32System.getWindowByPoint(point);
    }

    public Either<List<IProcess>> listProcessesWithName(final String name) {
        return win32System.listProcessesWithName(name);
    }

    public List<IWindow> findAllWindows(final String windowTitle, final String windowClassName) {
        return win32System.findAllWindows(windowTitle, windowClassName, true);
    }

    public List<IWindow> listAllWindows() {
        return win32System.listAllWindows();
    }

    public Optional<IWindow> getForegroundWindow() {
        return win32System.getForegroundWindow();
    }

    public IMouse getScreenMouse() {
        return win32System.getScreenMouse(DefaultSettings.MOUSE_SPEED_FACTOR);
    }

    public void minimizeAllWindows() {
        win32System.minimizeAllWindows();
    }

}
