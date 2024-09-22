package midorum.win32.deputy.common;

import com.midorum.win32api.hook.GlobalKeyHook;
import com.midorum.win32api.win32.Win32VirtualKey;
import midorum.win32.deputy.model.IExecutor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.time.Instant;
import java.time.ZoneId;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicLong;

public class UserActivityObserver {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final ConcurrentLinkedQueue<Integer> queue;
    private final AtomicLong lastUserKeyEventTime;

    public UserActivityObserver() {
        queue = new ConcurrentLinkedQueue<>();
        lastUserKeyEventTime = new AtomicLong();
    }

    public void checkKeyEvent(final GlobalKeyHook.KeyEvent event) {
        final Integer poll = queue.poll();
        final int adjustedCode = getAdjustedCode(event);
        if (poll == null || adjustedCode != poll) {
            lastUserKeyEventTime.set(System.currentTimeMillis());
            logger.trace(() -> ">>> poll: " + poll
                    + " event virtualCode: " + adjustedCode
                    + " set lastUserKeyEventTime: " + lastUserKeyEventTime
                    + " " + Instant.ofEpochMilli(lastUserKeyEventTime.get())
                    .atZone(ZoneId.systemDefault()).toLocalDateTime());
        }
    }

    private static int getAdjustedCode(final GlobalKeyHook.KeyEvent event) {
        final int eventCode = event.vkCode();
        return eventCode == Win32VirtualKey.VK_LMENU.code ? Win32VirtualKey.VK_MENU.code
                : eventCode == Win32VirtualKey.VK_RMENU.code ? Win32VirtualKey.VK_MENU.code
                : eventCode == Win32VirtualKey.VK_LCONTROL.code ? Win32VirtualKey.VK_CONTROL.code
                : eventCode == Win32VirtualKey.VK_RCONTROL.code ? Win32VirtualKey.VK_CONTROL.code
                : eventCode == Win32VirtualKey.VK_LSHIFT.code ? Win32VirtualKey.VK_SHIFT.code
                : eventCode == Win32VirtualKey.VK_RSHIFT.code ? Win32VirtualKey.VK_SHIFT.code
                : eventCode;
    }

    public long putSelfKeyCode(final int virtualCode) {
        final long l = lastUserKeyEventTime.get();
        if (l > 0) return l;
        queue.offer(virtualCode);
        return lastUserKeyEventTime.get();
    }

    public long getLastUserKeyEventTime() {
        return lastUserKeyEventTime.get();
    }

    public void reset() {
        queue.clear();
        lastUserKeyEventTime.set(0L);
    }
}
