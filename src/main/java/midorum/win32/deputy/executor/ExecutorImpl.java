package midorum.win32.deputy.executor;

import dma.validation.Validator;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Scenario;

import java.util.concurrent.*;
import java.util.function.Consumer;

public class ExecutorImpl implements IExecutor {

    private final InternalScheduledExecutor internalExecutor;

    public ExecutorImpl() {
        this.internalExecutor = new InternalScheduledExecutor();
    }

    @Override
    public void sendRoutineTask(final Scenario scenario, final Consumer<Throwable> errorHandler) {
        internalExecutor.scheduleWithFixedDelay(
                new CatchingRunnable(
                        new RoutineScenarioProcessor(Validator.checkNotNull(scenario).orThrowForSymbol("scenario")),
                        Validator.checkNotNull(errorHandler).orThrowForSymbol("task error handler")),
                0,
                1,
                TimeUnit.SECONDS);
    }

    @Override
    public void cancelCurrentTask() {
        internalExecutor.cancelCurrentTask();
    }

}
