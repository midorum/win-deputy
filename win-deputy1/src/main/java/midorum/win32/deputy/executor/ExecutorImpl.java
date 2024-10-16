package midorum.win32.deputy.executor;

import dma.validation.Validator;
import midorum.win32.deputy.common.GuardedWin32Adapter;
import midorum.win32.deputy.common.UserActivityObserver;
import midorum.win32.deputy.common.Win32Adapter;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.LogLevel;
import midorum.win32.deputy.model.Scenario;
import midorum.win32.deputy.model.Settings;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;

import java.io.File;
import java.util.concurrent.*;
import java.util.function.Consumer;

public class ExecutorImpl implements IExecutor {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final UserActivityObserver userActivityObserver;
    private final GuardedWin32Adapter win32Adapter;
    private final Settings settings;
    private final InternalScheduledExecutor internalExecutor;

    public ExecutorImpl(final UserActivityObserver userActivityObserver, final GuardedWin32Adapter win32Adapter, final Settings settings) {
        this.userActivityObserver = Validator.checkNotNull(userActivityObserver).orThrowForSymbol("userActivityObserver");
        this.win32Adapter = win32Adapter;
        this.settings = settings;
        this.internalExecutor = new InternalScheduledExecutor();
    }

    @Override
    public void sendRoutineTask(final File workingDirectory,
                                final Scenario scenario,
                                final Consumer<Throwable> errorHandler) {
        LogLevel.setRootLevel(settings.rootLogLevel());
        LogLevel.setLibLevel(settings.libLogLevel());
        logger.info("----------------------");
        logger.info("send routine task for execution");
        logger.info("settings: {}", settings);
        logger.info("----------------------");
        internalExecutor.scheduleWithFixedDelay(
                new CatchingRunnable(
                        new RoutineScenarioProcessor(workingDirectory, scenario, userActivityObserver, win32Adapter, settings),
                        errorHandler),
                settings.initialDelay(),
                settings.delay(),
                TimeUnit.SECONDS);
    }

    @Override
    public boolean cancelCurrentTask() {
        return internalExecutor.cancelCurrentTask();
    }

}
