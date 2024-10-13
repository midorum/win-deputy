package midorum.win32.deputy.model;

import midorum.win32.deputy.i18n.UiElement;

public class UserMessageException extends IllegalStateException {

    private final UiElement uiElement;
    private final Object[] args;

    public UserMessageException(final Throwable cause, final UiElement uiElement, Object... args) {
        super(uiElement.forDefaultLocale(args), cause);
        this.uiElement = uiElement;
        this.args = args;
    }

    public UserMessageException(final UiElement uiElement, Object... args) {
        super(uiElement.forDefaultLocale(args));
        this.uiElement = uiElement;
        this.args = args;
    }

    public UiElement getUiElement() {
        return uiElement;
    }

    public Object[] getArgs() {
        return args;
    }

}
