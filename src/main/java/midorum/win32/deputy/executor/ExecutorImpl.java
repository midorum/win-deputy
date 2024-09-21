package midorum.win32.deputy.executor;

import dma.validation.Validator;
import midorum.win32.deputy.common.Settings;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Scenario;

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
                Settings.INITIAL_DELAY,
                Settings.DELAY,
                TimeUnit.SECONDS);
    }

    @Override
    public boolean cancelCurrentTask() {
        return internalExecutor.cancelCurrentTask();
    }

}
