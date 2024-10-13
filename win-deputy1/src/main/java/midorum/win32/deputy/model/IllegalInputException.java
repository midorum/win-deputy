package midorum.win32.deputy.model;

import midorum.win32.deputy.i18n.UiElement;

public class IllegalInputException extends UserMessageException {

    public IllegalInputException(final Throwable cause, final UiElement uiElement, final Object... args) {
        super(cause, uiElement, args);
    }

    public IllegalInputException(final UiElement uiElement, final Object... args) {
        super(uiElement, args);
    }
}
