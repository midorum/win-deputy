package midorum.win32.deputy.executor;

import dma.util.Delay;
import dma.util.DurationFormatter;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

class RoutineScenarioProcessor implements Runnable {

    private final Logger logger = LogManager.getLogger(this);
    private final Scenario scenario;
    private final AtomicReference<Waiting> waiting = new AtomicReference<>();
    private final AtomicLong waitUntil = new AtomicLong(0);
    private final AtomicInteger waitIndex = new AtomicInteger(0);

    RoutineScenarioProcessor(final Scenario scenario) {
        System.out.println(">>> RoutineScenarioProcessor");
        this.scenario = scenario;
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
                if (!verifyChecks(activity.getChecks())) {
                    continue;
                }
                logger.info("activity {} ({}) has passed checks and start perform commands",
                        i, activity.getTitle());
                performCommands(activity.getCommands());
                final int index = i;
                activity.getWaiting().ifPresent(w -> {
                    waiting.set(w);
                    waitUntil.set(System.currentTimeMillis()
                            + TimeUnit.MILLISECONDS.convert(w.getTimeout(), TimeUnit.SECONDS));
                    waitIndex.set(index);
                });
                wasPerformed = true;
                break;
            }
            if (!wasPerformed) {
                final String message = "No one activity was performed. Maybe you've missed something. Interrupt scenario execution";
                logger.warn(message);
                //TODO make whole screenshot

                throw new UserMessageException(message);
            }
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
        logger.info("verifying checks");
        return false;
    }

    private void performCommands(final List<Command> commands) throws InterruptedException {

    }

    private void doRandomDelay() throws InterruptedException {
        new Delay(1)
                .randomSleep(0, 1, TimeUnit.MINUTES,
                        duration -> logger.info("routine task delayed on {}",
                                new DurationFormatter(duration).toStringWithoutZeroParts()));
        logger.info("routine task continue execution");
    }

}
