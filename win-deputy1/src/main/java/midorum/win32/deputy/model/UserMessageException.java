package midorum.win32.deputy.model;

public class UserMessageException extends IllegalStateException {
    public UserMessageException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public UserMessageException(String message) {
        super(message);
    }
}
