package midorum.win32.deputy.model;

public class IllegalInputException extends Exception {

    public IllegalInputException(String message) {
        super(message);
    }

    public IllegalInputException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
