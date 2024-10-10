package midorum.win32.deputy.common;

import com.midorum.win32api.facade.*;
import com.midorum.win32api.facade.Either;
import com.midorum.win32api.struct.PointInt;
import com.midorum.win32api.win32.MsLcid;
import midorum.win32.deputy.model.UserActionDetectedException;

import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Optional;

public class Win32Adapter {

    private final Win32System win32System;
    private final UserActivityObserver userActivityObserver;

    public Win32Adapter(final UserActivityObserver userActivityObserver) {
        this.userActivityObserver = userActivityObserver;
        win32System = Win32System.getInstance();
    }

    public BufferedImage takeScreenShot() {
        return Win32System.getInstance().getScreenShotMaker().takeWholeScreen();
    }

    public BufferedImage takeRectangleShot(final Rectangle rectangle) {
        return Win32System.getInstance().getScreenShotMaker().takeRectangle(rectangle);
    }

    public Either<MsLcid[]> getAvailableKeyboardLayouts() {
        return Win32System.getInstance().getAvailableKeyboardLayouts();
    }

    public Optional<WindowPoint> getWindowByPoint(final PointInt point) {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return Win32System.getInstance().getWindowByPoint(point);
    }

    public Either<List<IProcess>> listProcessesWithName(final String name) {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return Win32System.getInstance().listProcessesWithName(name);
    }

    public List<IWindow> findAllWindows(final String windowTitle, final String windowClassName) {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return Win32System.getInstance().findAllWindows(windowTitle, windowClassName, true);
    }

    public List<IWindow> listAllWindows() {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return Win32System.getInstance().listAllWindows();
    }

    public Optional<IWindow> getForegroundWindow() {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return win32System.getForegroundWindow();
    }

    public IMouse getScreenMouse() {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return Win32System.getInstance().getScreenMouse(DefaultSettings.MOUSE_SPEED_FACTOR);
    }

    public void minimizeAllWindows() {
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        win32System.minimizeAllWindows();
    }

}
