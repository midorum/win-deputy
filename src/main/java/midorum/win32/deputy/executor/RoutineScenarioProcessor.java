package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.Win32System;
import dma.util.Delay;
import dma.util.DurationFormatter;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

class RoutineScenarioProcessor implements Runnable {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final File workingDirectory;
    private final Scenario scenario;
    private final Win32Cache cache;
    private final CheckProcessor checkProcessor;
    private final AtomicReference<Waiting> waiting = new AtomicReference<>();
    private final AtomicLong waitUntil = new AtomicLong(0);
    private final AtomicInteger waitIndex = new AtomicInteger(0);

    RoutineScenarioProcessor(final File workingDirectory, final Scenario scenario) {
        this.workingDirectory = workingDirectory;
        this.scenario = scenario;
        this.cache = new Win32Cache(workingDirectory);
        this.checkProcessor = new CheckProcessor(cache);
    }

    @Override
    public void run() {
        try {
            logger.info("routine task started");
            //TODO check user activity on keyboard
            if (System.currentTimeMillis() < waitUntil.longValue() || !verifyWaitingChecks(waiting.get())) {
                //TODO propagate waiting message
                return;
            }
            final List<Activity> activities = scenario.getActivities();
            boolean wasPerformed = false;
            for (int i = 0; i < activities.size(); i++) {
                final Activity activity = activities.get(i);
                logger.info("verifying activity checks: {} ({})", i, activity.getTitle());
                if (!verifyChecks(activity.getChecks())) {
                    logger.info("activity {} ({}) has not passed checks and skip", i, activity.getTitle());
                    continue;
                }
                logger.info("activity {} ({}) has passed checks and start perform commands", i, activity.getTitle());
                performCommands(activity.getCommands());
                final int index = i;
                activity.getWaiting().ifPresent(w -> {
                    waiting.set(w);
                    waitUntil.set(System.currentTimeMillis() + TimeUnit.MILLISECONDS.convert(w.getTimeout(), TimeUnit.SECONDS));
                    waitIndex.set(index);
                });
                wasPerformed = true;
                break;
            }
            if (!wasPerformed) {
                final String message = "No one activity was performed. Maybe you've missed something. Interrupt scenario execution";
                final String fileNameForWrongShot = CommonUtil.getFileNameForWrongShot();
                logger.warn("{} ({})", message, fileNameForWrongShot);
                CommonUtil.saveImage(Win32System.getInstance().getScreenShotMaker().takeWholeScreen(),
                        CommonUtil.getPathForWrongShots(workingDirectory),
                        fileNameForWrongShot);
                throw new UserMessageException(message);
            }
            cache.invalidate();
//            doRandomDelay(); // maybe optionally
            logger.info("routine task done");
        } catch (InterruptedException e) {
            logger.warn(e.getMessage(), e);
            throw new ControlledInterruptedException(e);
        }
    }

    private boolean verifyWaitingChecks(final Waiting waiting) throws InterruptedException {
        return waiting == null || verifyChecks(waiting.getChecks());
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

    private void performCommands(final List<Command> commands) throws InterruptedException {
        for (Command command : commands) performCommand(command);
    }

    private void performCommand(final Command command) {
        logger.info("performing command: {}", command);
        final CommandType commandType = command.getType();
        if (commandType == CommandType.minimizeAllWindows) {
            Win32System.getInstance().minimizeAllWindows();
        } else {
            throw new UnsupportedOperationException();
        }
    }

    private void doRandomDelay() throws InterruptedException {
        new Delay(1).randomSleep(0, 1, TimeUnit.MINUTES, duration -> logger.info("routine task delayed on {}", new DurationFormatter(duration).toStringWithoutZeroParts()));
        logger.info("routine task continue execution");
    }

}
