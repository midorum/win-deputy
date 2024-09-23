package midorum.win32.deputy.executor;

import dma.validation.Validator;
import midorum.win32.deputy.common.Settings;
import midorum.win32.deputy.common.UserActivityObserver;
import midorum.win32.deputy.common.Win32Adapter;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Scenario;

import java.io.File;
import java.util.concurrent.*;
import java.util.function.Consumer;

public class ExecutorImpl implements IExecutor {

    private final UserActivityObserver userActivityObserver;
    private final Win32Adapter win32Adapter;
    private final InternalScheduledExecutor internalExecutor;

    public ExecutorImpl(final UserActivityObserver userActivityObserver, final Win32Adapter win32Adapter) {
        this.userActivityObserver = Validator.checkNotNull(userActivityObserver).orThrowForSymbol("userActivityObserver");
        this.win32Adapter = win32Adapter;
        this.internalExecutor = new InternalScheduledExecutor();
    }

    @Override
    public void sendRoutineTask(final File workingDirectory,
                                final Scenario scenario,
                                final Consumer<Throwable> errorHandler) {
        internalExecutor.scheduleWithFixedDelay(
                new CatchingRunnable(
                        new RoutineScenarioProcessor(workingDirectory, scenario, userActivityObserver, win32Adapter),
                        errorHandler),
                Settings.INITIAL_DELAY,
                Settings.DELAY,
                TimeUnit.SECONDS);
    }

    @Override
    public boolean cancelCurrentTask() {
        return internalExecutor.cancelCurrentTask();
    }

}
