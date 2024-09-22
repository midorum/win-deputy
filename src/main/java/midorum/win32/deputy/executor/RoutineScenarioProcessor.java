package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.Win32System;
import com.midorum.win32api.facade.exception.Win32ApiException;
import dma.util.Delay;
import dma.util.DurationFormatter;
import dma.validation.Validator;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.common.Settings;
import midorum.win32.deputy.common.UserActivityObserver;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Collections;
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
    private final Win32Cache cache;
    private final CheckProcessor checkProcessor;
    private final CommandProcessor commandProcessor;
    private final AtomicReference<WaitingList> waitingList = new AtomicReference<>();
    private final AtomicLong waitUntil = new AtomicLong(0);
    private final AtomicInteger repeatableIndex = new AtomicInteger(0);
    private final AtomicInteger repeatableCounter = new AtomicInteger(0);
    private final AtomicLong lastUserKeyEventTime = new AtomicLong(0);

    RoutineScenarioProcessor(final File workingDirectory, final Scenario scenario, final UserActivityObserver userActivityObserver) {
        this.workingDirectory = Validator.checkNotNull(workingDirectory).orThrowForSymbol("workingDirectory");
        this.scenario = Validator.checkNotNull(scenario).orThrowForSymbol("scenario");
        this.userActivityObserver = Validator.checkNotNull(userActivityObserver).orThrowForSymbol("userActivityObserver");
        this.cache = new Win32Cache(workingDirectory);
        this.checkProcessor = new CheckProcessor(cache);
        commandProcessor = new CommandProcessor(cache, userActivityObserver);
    }

    @Override
    public void run() {
        try {
            logger.info("routine task started");
            final long l = lastUserKeyEventTime.get();
            if (l > 0) {
                logger.info("scenario executing paused cause of user activity ({}) due to {}",
                        l,
                        Instant.ofEpochMilli(l + TimeUnit.MILLISECONDS.convert(Settings.USER_ACTIVITY_DELAY, TimeUnit.SECONDS))
                                .atZone(ZoneId.systemDefault()).toLocalDateTime());
                TimeUnit.SECONDS.sleep(Settings.USER_ACTIVITY_DELAY);
                lastUserKeyEventTime.set(0);
            }
            final WaitingList setWaitingList = waitingList.get();
            if (!verifyWaitingList(setWaitingList) && System.currentTimeMillis() < waitUntil.longValue()) {
                logger.info("waiting list has not passed checks and waiting time does not expired - interrupt routine task");
                cache.invalidate();
                return;
            } else if (setWaitingList != null) {
                logger.info("waiting has done - continue performing activities");
                waitingList.set(null);
                waitUntil.set(0);
            }
            final List<Activity> activities = scenario.getActivities();
            int wasPerformed = -1;
            for (int i = 0; i < activities.size(); i++) {
                final Activity activity = activities.get(i);
                logger.info("verifying activity checks: {} (\"{}\")", i, activity.getTitle());
                if (!verifyChecks(activity.getChecks())) {
                    logger.info("activity \"{}\" ({}) has not passed checks and skip", i, activity.getTitle());
                    continue;
                }
                logger.info("activity {} (\"{}\") has passed checks and start perform commands", i, activity.getTitle());
                performCommands(activity.getCommands());
                final int currentIndex = i;
                if (!activity.isRepeatable() && repeatableCounter.updateAndGet(operand ->
                        currentIndex == repeatableIndex.getAndSet(currentIndex) ? operand + 1 : 0) == Settings.MAX_REPEATABLE_COUNT) {
                    throw makeShotAndGetException("Activity \"" + activity.getTitle() + "\" (" + currentIndex + ")" +
                            " was executed " + Settings.MAX_REPEATABLE_COUNT + " times in a row but it's not marked as repeatable." +
                            " Maybe you've missed something. Interrupt scenario execution");
                }
                final int index = i;
                activity.getWaitingList().ifPresent(waitingList -> {
                    this.waitingList.set(waitingList);
                    final long waitUntilValue = System.currentTimeMillis() + TimeUnit.MILLISECONDS.convert(waitingList.getTimeout(), TimeUnit.SECONDS);
                    waitUntil.set(waitUntilValue);
                    logger.info("activity {} (\"{}\") set waiting list \"{}\" until {}",
                            index,
                            activity.getTitle(),
                            waitingList.getList().stream().map(Waiting::getDescription).toList(),
                            Instant.ofEpochMilli(waitUntilValue).atZone(ZoneId.systemDefault()).toLocalDateTime());
                });
                wasPerformed = i;
                break;
            }
            if (wasPerformed < 0) {
                throw makeShotAndGetException("No one activity was performed. Maybe you've missed something. Interrupt scenario execution");
            }
            if (scenario.getType() == ScenarioType.oneTime && wasPerformed >= activities.size() - 1) {
                throw new UserMessageException("Scenario \"" + scenario.getTitle() + "\" done its job");
            }
            cache.invalidate();
            if (scenario.getType() == ScenarioType.repeatable && wasPerformed >= activities.size() - 1) {
                @SuppressWarnings("unchecked") final Map<ScenarioDataType, String> data = scenario.getData().orElse(Collections.EMPTY_MAP);
                final String repeatDelayValue = data.get(ScenarioDataType.repeatDelay);
                if (repeatDelayValue != null) {
                    ScenarioDataType.repeatDelay.getUnit().orElse(Settings.DEFAULT_TIME_UNIT)
                            .sleep(Long.parseLong(repeatDelayValue));
                }
            }
//            doRandomDelay(); // maybe optionally
            logger.info("routine task done");
        } catch (UserActionDetectedException e) {
            lastUserKeyEventTime.set(userActivityObserver.getLastUserKeyEventTime());
            cache.invalidate();
            userActivityObserver.reset();
            repeatableIndex.set(0);
            repeatableCounter.set(0);
            logger.info("routine interrupted cause of user activity");
        } catch (InterruptedException e) {
            logger.warn(e.getMessage(), e);
            throw new ControlledInterruptedException(e);
        } catch (Win32ApiException e) {
            throw new IllegalStateException(e);
        }
    }

    private UserMessageException makeShotAndGetException(final String message) {
        final String fileNameForWrongShot = CommonUtil.getFileNameForWrongShot();
        logger.warn("{} ({})", message, fileNameForWrongShot);
        CommonUtil.saveImage(Win32System.getInstance().getScreenShotMaker().takeWholeScreen(),
                CommonUtil.getPathForWrongShots(workingDirectory),
                fileNameForWrongShot);
        return new UserMessageException(message);
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
        logger.info("verifying check: {}", check);
        return checkProcessor.verifyCheck(check);
    }

    private void performCommands(final List<Command> commands) throws InterruptedException, Win32ApiException {
        for (Command command : commands) performCommand(command);
    }

    private void performCommand(final Command command) throws Win32ApiException, InterruptedException {
        logger.info("performing command: {}", command);
        commandProcessor.performCommand(command);
    }

    private void doRandomDelay() throws InterruptedException {
        new Delay(1).randomSleep(0, 1, TimeUnit.MINUTES, duration -> logger.info("routine task delayed on {}", new DurationFormatter(duration).toStringWithoutZeroParts()));
        logger.info("routine task continue execution");
    }

}
