package midorum.win32.deputy.executor;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.concurrent.*;

class InternalScheduledExecutor {

    private final Logger logger = LogManager.getLogger(this);
    private final ScheduledExecutorService executor;
    private volatile Future<?> future;

    InternalScheduledExecutor() {
        executor = Executors.newSingleThreadScheduledExecutor();
        setShutdownHook();
    }

    public void scheduleWithFixedDelay(Runnable command,
                                       long initialDelay,
                                       long delay,
                                       TimeUnit unit) {
        submitTask(command, initialDelay, delay, unit);
        logger.info("task submitted");
    }

    public boolean cancelCurrentTask() {
        final boolean result = cancelTask();
        if (result)
            logger.info("task cancelled");
        return result;
    }

    private synchronized void submitTask(final Runnable command, final long initialDelay, final long delay, final TimeUnit unit) {
        cancelTask();
        this.future = executor.scheduleWithFixedDelay(command, initialDelay, delay, unit);
    }

    private synchronized boolean cancelTask() {
        if (this.future == null || this.future.isDone()) return false;
        return this.future.cancel(true);
    }

    private void setShutdownHook() {
        class Hook extends Thread {
            @Override
            public void run() {
                logger.info("await termination");
                awaitTerminationAndShutdown(executor);
                logger.info("executor services terminated");
            }
        }
        Runtime.getRuntime().addShutdownHook(new Hook());
        logger.info("shutdown hook added");
    }

    private void awaitTerminationAndShutdown(ExecutorService pool) {
        pool.shutdown(); // Disable new tasks from being submitted
        try {
            // Wait a while for existing tasks to terminate
            if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
                pool.shutdownNow(); // Cancel currently executing tasks
                // Wait a while for tasks to respond to being cancelled
                if (!pool.awaitTermination(60, TimeUnit.SECONDS))
                    logger.error("Pool did not terminate");
            }
        } catch (InterruptedException ie) {
            // (Re-)Cancel if current thread also interrupted
            pool.shutdownNow();
            // Preserve interrupt status
            Thread.currentThread().interrupt();
        }
    }

}
