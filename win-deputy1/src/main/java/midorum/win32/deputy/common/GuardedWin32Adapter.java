package midorum.win32.deputy.common;

import com.midorum.win32api.facade.*;
import com.midorum.win32api.facade.Either;
import com.midorum.win32api.struct.PointInt;
import midorum.win32.deputy.model.UserActionDetectedException;

import java.util.List;
import java.util.Optional;

public class GuardedWin32Adapter extends Win32Adapter {

    private final UserActivityObserver userActivityObserver;

    public GuardedWin32Adapter(final UserActivityObserver userActivityObserver) {
        this.userActivityObserver = userActivityObserver;
    }

    public Optional<WindowPoint> getWindowByPoint(final PointInt point) {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return super.getWindowByPoint(point);
    }

    public Either<List<IProcess>> listProcessesWithName(final String name) {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return super.listProcessesWithName(name);
    }

    public List<IWindow> findAllWindows(final String windowTitle, final String windowClassName) {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return super.findAllWindows(windowTitle, windowClassName);
    }

    public List<IWindow> listAllWindows() {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return super.listAllWindows();
    }

    public Optional<IWindow> getForegroundWindow() {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return super.getForegroundWindow();
    }

    public IMouse getScreenMouse() {
        //FIXME we have to check user activity so early because this method returns a common interface; we need return an adapter here
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        return super.getScreenMouse();
    }

    public void minimizeAllWindows() {
        if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
        super.minimizeAllWindows();
    }

}
