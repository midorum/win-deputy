package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.exception.Win32ApiException;
import dma.util.Delay;
import dma.util.DurationFormatter;
import dma.validation.Validator;
import midorum.win32.deputy.common.*;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

class RoutineScenarioProcessor implements Runnable {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final File workingDirectory;
    private final Scenario scenario;
    private final UserActivityObserver userActivityObserver;
    private final GuardedWin32Adapter win32Adapter;
    private final Settings settings;
    private final Win32Cache cache;
    private final CheckProcessor checkProcessor;
    private final CommandProcessor commandProcessor;
    private final AtomicReference<WaitingList> waitingList = new AtomicReference<>();
    private final AtomicLong waitUntil = new AtomicLong(0);
    private final AtomicInteger repeatableIndex = new AtomicInteger(-1);
    private final AtomicInteger repeatableCounter = new AtomicInteger(0);
    private final AtomicLong lastUserKeyEventTime = new AtomicLong(0);
    private final AtomicBoolean beGentle = new AtomicBoolean(false);

    RoutineScenarioProcessor(final File workingDirectory,
                             final Scenario scenario,
                             final UserActivityObserver userActivityObserver,
                             final GuardedWin32Adapter win32Adapter,
                             final Settings settings) {
        this.workingDirectory = Validator.checkNotNull(workingDirectory).orThrowForSymbol("workingDirectory");
        this.scenario = Validator.checkNotNull(scenario).orThrowForSymbol("scenario");
        this.userActivityObserver = Validator.checkNotNull(userActivityObserver).orThrowForSymbol("userActivityObserver");
        this.win32Adapter = Validator.checkNotNull(win32Adapter).orThrowForSymbol("win32Adapter");
        this.settings = Validator.checkNotNull(settings).orThrowForSymbol("settings");
        this.cache = new Win32Cache(workingDirectory, win32Adapter, settings);
        this.checkProcessor = new CheckProcessor(cache);
        this.commandProcessor = new CommandProcessor(cache, userActivityObserver, win32Adapter);
    }

    @Override
    public void run() {
        try {
            logger.info("routine task started {}", beGentle.get() ? "(gentle mode)" : "");
            final long lastUserActivityTime = lastUserKeyEventTime.get();
            if (lastUserActivityTime > 0) {
                cache.invalidate();
                userActivityObserver.reset();
                repeatableIndex.set(-1);
                repeatableCounter.set(0);
                lastUserKeyEventTime.set(0);
                logger.info("scenario executing paused cause of user activity ({}) due to {}",
                        lastUserActivityTime,
                        CommonUtil.localTimeString(System.currentTimeMillis()
                                + TimeUnit.MILLISECONDS.convert(settings.userActivityDelay(), TimeUnit.SECONDS)));
                TimeUnit.SECONDS.sleep(settings.userActivityDelay());
            }
            if (userActivityObserver.wasUserActivity()) throw new UserActionDetectedException();
            final WaitingList setWaitingList = waitingList.get();
            final boolean waitingTimeIsOut = System.currentTimeMillis() > waitUntil.longValue();
            if (!verifyWaitingList(setWaitingList) && !waitingTimeIsOut) {
                logger.info("waiting list has not passed checks and waiting time does not expired - interrupt routine task");
                cache.invalidate();
                return;
            } else if (waitingTimeIsOut) {
                makeShot("waiting has done by timeout - continue performing activities");
                waitingList.set(null);
                waitUntil.set(0);
            } else if (setWaitingList != null) {
                logger.info("waiting has done - continue performing activities");
                waitingList.set(null);
                waitUntil.set(0);
            }
            final List<Activity> activities = scenario.getActivities();
            int wasPerformed = -1;
            for (int i = 0; i < activities.size(); i++) {
                final Activity activity = activities.get(i);
                if (activity.isIgnore()) {
                    logger.info("activity {} (\"{}\") marked as ignored - skip it", i, activity.getTitle());
                    continue;
                }
                logger.info("verifying activity checks: {} (\"{}\")", i, activity.getTitle());
                if (!verifyChecks(activity.getChecks())) {
                    logger.info("activity \"{}\" ({}) has not passed checks and skip", i, activity.getTitle());
                    continue;
                }
                logger.info("activity {} (\"{}\") has passed checks and start perform commands", i, activity.getTitle());
                performCommands(activity.getCommands());
                final int currentIndex = i;
                final int activityRepeated = repeatableCounter.updateAndGet(operand ->
                        currentIndex == repeatableIndex.getAndSet(currentIndex) ? operand + 1 : 0);
                if (!activity.isRepeatable() && activityRepeated >= settings.maxRepeatableCount()) {
                    throw makeShotAndGetException("Activity \"" + activity.getTitle() + "\" (" + currentIndex + ")" +
                            " was executed " + settings.maxRepeatableCount() + " times in a row but it's not marked as repeatable." +
                            " Maybe you've missed something. Interrupt scenario execution");
                }
                if (activity.producesFragileState()) {
                    logger.warn("Activity {} (\"{}\") marked as produces a fragile state. So try to perform next round gently.",
                            i, activity.getTitle());
                    beGentle.compareAndSet(false, true);
                } else if (!activity.isRepeatable() && activityRepeated > 0) {
                    logger.warn("Activity {} (\"{}\") was repeated two or more times in a row but it's not marked as repeatable." +
                                    " Maybe we lost some subtle state. So try to perform next round gently.",
                            i, activity.getTitle());
                    beGentle.compareAndSet(false, true);
                } else if (activityRepeated == 0) {
                    beGentle.compareAndSet(true, false);
                }
                final int index = i;
                activity.getWaitingList().ifPresent(waitingList -> {
                    this.waitingList.set(waitingList);
                    final long waitUntilValue = System.currentTimeMillis()
                            + TimeUnit.MILLISECONDS.convert(waitingList.getTimeout(), TimeUnit.SECONDS);
                    waitUntil.set(waitUntilValue);
                    logger.info("activity {} (\"{}\") set waiting list \"{}\" until {}",
                            index,
                            activity.getTitle(),
                            waitingList.getList().stream().map(Waiting::getDescription).toList(),
                            CommonUtil.localTimeString(waitUntilValue));
                });
                wasPerformed = i;
                break;
            }
            cache.invalidate();
            if (wasPerformed < 0) {
                throw makeShotAndGetException("No one activity was performed. Maybe you've missed something. Interrupt scenario execution");
            }
            if (wasPerformed < activities.size() - 1) {
                logger.info("routine task done");
                return;
            }
            if (scenario.getType() == ScenarioType.oneTime) {
                throw new UserMessageException("Scenario \"" + scenario.getTitle() + "\" done its job");
            }
            if (scenario.getType() == ScenarioType.repeatable) {
                final Map<ScenarioDataType, String> data = scenario.getData().orElse(Map.of());
                final String repeatDelayValue = data.get(ScenarioDataType.repeatDelay);
                if (repeatDelayValue != null) {
                    final TimeUnit timeUnit = ScenarioDataType.repeatDelay.getUnit().orElse(DefaultSettings.DEFAULT_TIME_UNIT);
                    logger.info("scenario marked as repeatable with {} {} delay - perform delay",
                            repeatDelayValue, timeUnit.name().toLowerCase());
                    timeUnit.sleep(Long.parseLong(repeatDelayValue));
                }
                final String randomDelayData = data.get(ScenarioDataType.randomDelay);
                if (randomDelayData != null) {
                    new Delay(1).randomSleep(0, Long.parseLong(randomDelayData),
                            ScenarioDataType.randomDelay.getUnit().orElse(DefaultSettings.DEFAULT_TIME_UNIT),
                            duration -> logger.info("scenario asked random delay - routine task delayed on {}",
                                    new DurationFormatter(duration).toStringWithoutZeroParts()));
                }
            }
        } catch (UserActionDetectedException e) {
            lastUserKeyEventTime.set(userActivityObserver.getLastUserKeyEventTime());
            logger.warn("routine interrupted cause of user activity");
        } catch (InterruptedException e) {
            logger.warn(e.getMessage(), e);
            throw new ControlledInterruptedException(e);
        } catch (Win32ApiException e) {
            throw new IllegalStateException(e);
        }
    }

    private UserMessageException makeShotAndGetException(final String message) {
        makeShot(message);
        return new UserMessageException(message);
    }

    private void makeShot(final String message) {
        final String fileNameForWrongShot = CommonUtil.getFileNameForWrongShot();
        logger.warn("{} ({})", message, fileNameForWrongShot);
        CommonUtil.saveImage(win32Adapter.takeScreenShot(),
                CommonUtil.getPathForWrongShots(workingDirectory),
                fileNameForWrongShot);
    }

    private boolean verifyWaitingList(final WaitingList waitingList) throws InterruptedException {
        if (waitingList == null) return true;
        logger.info("verifying waitingList \"{}\"", waitingList.getList().stream().map(Waiting::getDescription).toList());
        boolean result = false;
        for (Waiting waiting : waitingList.getList()) {
            if (verifyWaitingChecks(waiting)) {
                logger.info("waiting \"{}\" has done", waiting.getDescription());
                result = true;
                break;
            }
            logger.info("waiting \"{}\" has not passed checks", waiting.getDescription());
        }
        return result;
    }

    private boolean verifyWaitingChecks(final Waiting waiting) throws InterruptedException {
        if (waiting == null) return true;
        logger.info("verifying waiting \"{}\"", waiting.getDescription());
        return verifyChecks(waiting.getChecks());
    }

    private boolean verifyChecks(final List<Check> checks) throws InterruptedException {
        // using vanilla 'for' to ensure break verifying at first false result
        boolean result = true;
        for (Check check : checks) {
            if (!verifyCheck(check)) {
                result = false;
                break;
            }
        }
        return result;
    }

    private boolean verifyCheck(final Check check) {
        if (check.isIgnore()) {
            logger.info("check {} marked as ignored - skip it", check);
            return true;
        }
        logger.info("verifying check: {}", check);
        return checkProcessor.verifyCheck(check, beGentle.get());
    }

    private void performCommands(final List<Command> commands) throws InterruptedException, Win32ApiException {
        for (Command command : commands) performCommand(command);
    }

    private void performCommand(final Command command) throws Win32ApiException, InterruptedException {
        logger.info("performing command: {}", command);
        commandProcessor.performCommand(command);
    }

}
