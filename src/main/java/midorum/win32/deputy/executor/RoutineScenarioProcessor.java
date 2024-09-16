package midorum.win32.deputy.executor;

import dma.util.Delay;
import dma.util.DurationFormatter;
import midorum.win32.deputy.model.ControlledInterruptedException;
import midorum.win32.deputy.model.Scenario;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.concurrent.TimeUnit;

class RoutineScenarioProcessor implements Runnable {

    private final Logger logger = LogManager.getLogger(this);
    private final Scenario scenario;

    RoutineScenarioProcessor(final Scenario scenario) {
        this.scenario = scenario;
    }

    @Override
    public void run() {
//        try {
            logger.info("routine task started");
            //some work
            System.out.println(scenario);
//            doRandomDelay(); // maybe optionally
            logger.info("routine task done");
//        } catch (InterruptedException e) {
//            logger.warn(e.getMessage(), e);
//            throw new ControlledInterruptedException(e);
//        }
    }

    private void doRandomDelay() throws InterruptedException {
        new Delay(1)
                .randomSleep(0, 1, TimeUnit.MINUTES,
                        duration -> logger.info("routine task delayed on {}",
                                new DurationFormatter(duration).toStringWithoutZeroParts()));
        logger.info("routine task continue execution");
    }

}
