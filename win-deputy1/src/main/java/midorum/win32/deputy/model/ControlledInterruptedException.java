package midorum.win32.deputy.model;

public class ControlledInterruptedException extends RuntimeException {

    public ControlledInterruptedException(Throwable cause) {
        super(cause);
    }

    public ControlledInterruptedException(String message, Throwable cause) {
        super(message, cause);
    }
}
