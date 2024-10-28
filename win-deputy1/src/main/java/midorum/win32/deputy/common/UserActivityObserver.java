package midorum.win32.deputy.common;

import com.midorum.win32api.facade.HotKey;
import com.midorum.win32api.hook.GlobalKeyHook;
import com.midorum.win32api.hook.KeyHookHelper;
import com.midorum.win32api.win32.Win32VirtualKey;
import dma.function.VoidAction;
import dma.validation.Validator;
import midorum.win32.deputy.model.IExecutor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicLong;

public class UserActivityObserver {

    private final static UserActivityObserver INSTANCE = new UserActivityObserver();
    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final ConcurrentLinkedQueue<Integer> queue;
    private final AtomicLong lastUserKeyEventTime;
    private volatile boolean observationEnabled = false;
    private final ConcurrentHashMap<HotKey, Map<String, VoidAction>> keyEventListeners;

    private UserActivityObserver() {
        queue = new ConcurrentLinkedQueue<>();
        lastUserKeyEventTime = new AtomicLong();
        keyEventListeners = new ConcurrentHashMap<>();
        setKeyHook();
    }

    public static UserActivityObserver getInstance() {
        return INSTANCE;
    }

    private void setKeyHook() {
        final KeyHookHelper keyHookHelper = KeyHookHelper.getInstance();
        final String hookId = Long.toString(System.currentTimeMillis());
        keyHookHelper.setGlobalHook(hookId,
                keyEvent -> {
                    if (observationEnabled) {
                        logger.trace(() -> "GlobalKeyHook[" + hookId + "] keyEvent: " + keyEvent
                                + " " + keyEvent.toPrettyString());
                        checkKeyEvent(keyEvent);
                    }
                    if (keyEvent.isKeyUp()) {
                        final Map<String, VoidAction> actions = keyEventListeners.get(HotKey.fromKeyEvent(keyEvent));
                        if (actions != null) {
                            actions.forEach((_, action) -> action.perform());
                        }
                    }
                    return KeyHookHelper.Result.propagateEvent;
                },
                throwable -> {
                    logger.error(() -> "GlobalKeyHook[" + hookId + "] error", throwable);
                    return KeyHookHelper.Result.keepHook;
                }
        );
    }

    private void checkKeyEvent(final GlobalKeyHook.KeyEvent event) {
        final Integer poll = queue.poll();
        final int adjustedCode = getAdjustedCode(event);
        if (poll == null || adjustedCode != poll) {
            lastUserKeyEventTime.set(System.currentTimeMillis());
            logger.trace(() -> "polled self code [" + poll
                    + "] != event virtualCode [" + adjustedCode
                    + "]: set lastUserKeyEventTime: " + lastUserKeyEventTime
                    + " [" + CommonUtil.localTimeString(lastUserKeyEventTime.get()) + "]");
        }
    }

    private int getAdjustedCode(final GlobalKeyHook.KeyEvent event) {
        final int eventCode = event.vkCode();
        return eventCode == Win32VirtualKey.VK_LMENU.code ? Win32VirtualKey.VK_MENU.code
                : eventCode == Win32VirtualKey.VK_RMENU.code ? Win32VirtualKey.VK_MENU.code
                : eventCode == Win32VirtualKey.VK_LCONTROL.code ? Win32VirtualKey.VK_CONTROL.code
                : eventCode == Win32VirtualKey.VK_RCONTROL.code ? Win32VirtualKey.VK_CONTROL.code
                : eventCode == Win32VirtualKey.VK_LSHIFT.code ? Win32VirtualKey.VK_SHIFT.code
                : eventCode == Win32VirtualKey.VK_RSHIFT.code ? Win32VirtualKey.VK_SHIFT.code
                : eventCode;
    }

    public void enableObservation() {
        this.observationEnabled = true;
        logger.info("user observation has been enabled");
    }

    public void disableObservation() {
        this.observationEnabled = false;
        reset();
        logger.info("user observation has been disabled");
    }

    public void addKeyListener(final String consumer, final HotKey hotKey, final VoidAction action) {
        final String consumerChecked = Validator.checkNotNull(consumer).andNot(String::isBlank).orThrowForSymbol("consumer");
        final VoidAction actionChecked = Validator.checkNotNull(action).orThrowForSymbol("action");
        keyEventListeners.compute(Validator.checkNotNull(hotKey).orThrowForSymbol("hotKey"), (_, map) -> {
            final Map<String, VoidAction> actions = map != null ? map : new HashMap<>();
            actions.put(consumerChecked, actionChecked);
            return actions;
        });
    }

    public boolean wasUserActivity() {
        return lastUserKeyEventTime.get() > 0;
    }

    public boolean wasUserActivity(final int virtualCode) {
        final boolean wasUserActivity = wasUserActivity();
        if (!wasUserActivity) {
            queue.offer(virtualCode);
            return false;
        }
        return true;
    }

    public long getLastUserKeyEventTime() {
        return lastUserKeyEventTime.get();
    }

    public void reset() {
        queue.clear();
        lastUserKeyEventTime.set(0L);
    }
}
