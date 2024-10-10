package midorum.win32.deputy.executor;

import dma.validation.Validator;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.function.Consumer;

public class CatchingRunnable implements Runnable {

    private final Logger logger = LogManager.getLogger(this);
    private final Runnable delegate;
    private final Consumer<Throwable> errorHandler;

    public CatchingRunnable(final Runnable delegate, final Consumer<Throwable> errorHandler) {
        this.delegate = Validator.checkNotNull(delegate).orThrowForSymbol("delegate");
        this.errorHandler = Validator.checkNotNull(errorHandler).orThrowForSymbol("task error handler");
    }

    @Override
    public void run() {
        try {
            delegate.run();
        } catch (Throwable t) {
            this.errorHandler.accept(t); // handling exception
            throw t; // break task execution
        }

    }
}
