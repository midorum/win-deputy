package midorum.win32.deputy.model;

import java.io.File;
import java.util.function.Consumer;

public interface IExecutor {

    String LOGGER_NAME = "executor";

    void sendRoutineTask(final File workingDirectory, final Scenario scenario, final Consumer<Throwable> errorHandler);

    boolean cancelCurrentTask();
}
