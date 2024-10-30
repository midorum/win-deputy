package midorum.win32.deputy.common;

import com.midorum.win32api.facade.HotKey;
import com.midorum.win32api.hook.*;
import com.midorum.win32api.win32.Win32Event;
import com.midorum.win32api.win32.Win32VirtualKey;
import dma.function.VoidAction;
import dma.validation.Validator;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Settings;
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
    private final boolean observeMouse;
    private final ConcurrentHashMap<HotKey, Map<String, VoidAction>> keyEventListeners;

    private UserActivityObserver() {
        queue = new ConcurrentLinkedQueue<>();
        lastUserKeyEventTime = new AtomicLong();
        keyEventListeners = new ConcurrentHashMap<>();
        final Settings settings = loadSettings();
        if (settings.observeMouseButtons()) {
            setMouseHook();
            observeMouse = true;
        } else observeMouse = false;
        setKeyHook();
    }

    public static UserActivityObserver getInstance() {
        return INSTANCE;
    }

    private Settings loadSettings() {
        return Settings.loadFromFile().getOrHandleError(e -> {
            logger.error("cannot load settings from file", e);
            return Settings.defaultSettings();
        });
    }

    private void setKeyHook() {
        final KeyHookHelper keyHookHelper = KeyHookHelper.getInstance();
        final String hookId = "observer.hook.key_" + System.currentTimeMillis();
        keyHookHelper.setGlobalHook(hookId,
                event -> {
                    if (observationEnabled) {
                        logger.trace(() -> "[" + hookId + "] event: " + event
                                + " " + event.toPrettyString());
                        checkKeyEvent(event);
                    }
                    if (event.isKeyUp()) {
                        final Map<String, VoidAction> actions = keyEventListeners.get(HotKey.fromKeyEvent(event));
                        if (actions != null) {
                            actions.forEach((_, action) -> action.perform());
                        }
                    }
                    return EventProcessingResult.propagateEvent;
                },
                throwable -> {
                    logger.error(() -> "[" + hookId + "] error", throwable);
                    return EventProcessingResult.keepHook;
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

    private void setMouseHook() {
        final MouseHookHelper mouseHookHelper = MouseHookHelper.getInstance();
        final String hookId = "observer.hook.mouse_" + System.currentTimeMillis();
        mouseHookHelper.setGlobalHookOnKeys(hookId,
                event -> {
                    if (observationEnabled) {
                        logger.trace(() -> "[" + hookId + "] event: " + event
                                + " " + event.toPrettyString());
                        checkMouseEvent(event);
                    }
                    return EventProcessingResult.propagateEvent;
                },
                throwable -> {
                    logger.error(() -> "[" + hookId + "] error", throwable);
                    return EventProcessingResult.keepHook;
                }
        );
    }

    private void checkMouseEvent(final GlobalMouseHook.MouseEvent event) {
        final Integer poll = queue.poll();
        final int eventType = event.eventType();
        if (poll == null || eventType != poll) {
            lastUserKeyEventTime.set(System.currentTimeMillis());
            logger.trace(() -> "polled self event type [" + poll
                    + "] != event type [" + eventType
                    + "]: set lastUserKeyEventTime: " + lastUserKeyEventTime
                    + " [" + CommonUtil.localTimeString(lastUserKeyEventTime.get()) + "]");
        }
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

    public boolean checkUserActivityOrPutSelfKeyCode(final int virtualCode) {
        if (wasUserActivity()) return true;
        queue.offer(virtualCode);
        return false;
    }

    public boolean checkUserActivityOrPutSelfEvent(final Win32Event selfEvent) {
        if (wasUserActivity()) return true;
        queue.offer(selfEvent.getMessage());
        return false;
    }

    public boolean checkUserActivityOrPutSelfMouseEvent(final Win32Event selfEvent) {
        if (!observeMouse) return false;
        return checkUserActivityOrPutSelfEvent(selfEvent);
    }

    public long getLastUserKeyEventTime() {
        return lastUserKeyEventTime.get();
    }

    public void reset() {
        queue.clear();
        lastUserKeyEventTime.set(0L);
    }
}
