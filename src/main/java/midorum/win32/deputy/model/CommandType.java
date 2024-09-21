package midorum.win32.deputy.model;

import java.util.Collections;
import java.util.List;

import static midorum.win32.deputy.model.CommandDataType.*;

public enum CommandType {
    minimizeAllWindows(Collections.EMPTY_LIST, Collections.EMPTY_LIST),
    mouseLeftClick(Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Collections.EMPTY_LIST),
    mouseDoubleLeftClick(Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Collections.EMPTY_LIST),
    mouseRightClick(Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Collections.EMPTY_LIST),
    mouseDoubleRightClick(Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Collections.EMPTY_LIST),
    keyboardType(List.of(List.of(keyboardTypeText)), List.of(keyboardLayout, windowTitle, windowClassName)),
    keyboardHitKey(List.of(List.of(keyboardKeyStroke)), List.of(keyboardKeyStrokeDelay, keyboardLayout, windowTitle, windowClassName));

    private final List<List<CommandDataType>> mandatory;
    private final List<CommandDataType> optional;

    CommandType(final List<List<CommandDataType>> mandatory, final List<CommandDataType> optional) {
        this.mandatory = mandatory; //unmodifiable list
        this.optional = optional; //unmodifiable list
    }

    public List<List<CommandDataType>> mandatory() {
        return this.mandatory;
    }

    public List<CommandDataType> optional() {
        return this.optional;
    }

    private final static class Constants {
        public static final List<List<CommandDataType>> MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES = List.of(List.of(mousePosition), List.of(mouseShotRelatedPosition));
    }

}
