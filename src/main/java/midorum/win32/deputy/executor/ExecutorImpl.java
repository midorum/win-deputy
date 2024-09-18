package midorum.win32.deputy.executor;

import dma.validation.Validator;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Scenario;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.concurrent.*;
import java.util.function.Consumer;

public class ExecutorImpl implements IExecutor {

    private final InternalScheduledExecutor internalExecutor;

    public ExecutorImpl() {
        this.internalExecutor = new InternalScheduledExecutor();
    }

    @Override
    public void sendRoutineTask(final File workingDirectory, final Scenario scenario, final Consumer<Throwable> errorHandler) {
        internalExecutor.scheduleWithFixedDelay(
                new CatchingRunnable(
                        new RoutineScenarioProcessor(Validator.checkNotNull(workingDirectory).orThrowForSymbol("workingDirectory"),
                                Validator.checkNotNull(scenario).orThrowForSymbol("scenario")),
                        Validator.checkNotNull(errorHandler).orThrowForSymbol("task error handler")),
                5,
                1,
                TimeUnit.SECONDS);
    }

    @Override
    public boolean cancelCurrentTask() {
        return internalExecutor.cancelCurrentTask();
    }

}
