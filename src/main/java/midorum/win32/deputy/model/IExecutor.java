package midorum.win32.deputy.model;

import java.util.function.Consumer;

public interface IExecutor {

    void sendRoutineTask(final Scenario scenario, final Consumer<Throwable> errorHandler);

    void cancelCurrentTask();
}
