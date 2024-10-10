package midorum.win32.deputy.model;

import java.util.List;
import java.util.Optional;

import static midorum.win32.deputy.model.CommandDataType.*;

public enum CommandType {
    minimizeAllWindows(null, List.of(), List.of()),
    mouseLeftClickAbs(null, Constants.MOUSE_CLICK_ABS_MANDATORY_COMMAND_DATA_TYPES, List.of()),
    mouseDoubleLeftClickAbs(null, Constants.MOUSE_CLICK_ABS_MANDATORY_COMMAND_DATA_TYPES, List.of()),
    mouseRightClickAbs(null, Constants.MOUSE_CLICK_ABS_MANDATORY_COMMAND_DATA_TYPES, List.of()),
    mouseDoubleRightClickAbs(null, Constants.MOUSE_CLICK_ABS_MANDATORY_COMMAND_DATA_TYPES, List.of()),
    mouseLeftClickOnStamp(CheckType.stampFound, Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Constants.MOUSE_CLICK_OPTIONAL_COMMAND_DATA_TYPES),
    mouseDoubleLeftClickOnStamp(CheckType.stampFound, Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Constants.MOUSE_CLICK_OPTIONAL_COMMAND_DATA_TYPES),
    mouseRightClickOnStamp(CheckType.stampFound, Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Constants.MOUSE_CLICK_OPTIONAL_COMMAND_DATA_TYPES),
    mouseDoubleRightClickOnStamp(CheckType.stampFound, Constants.MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES, Constants.MOUSE_CLICK_OPTIONAL_COMMAND_DATA_TYPES),
    keyboardHitKey(null, List.of(keyboardKeyStroke), List.of(keyboardKeyStrokeDelay, keyboardLayout)),
    keyboardHitKeyInWindow(CheckType.windowExists, List.of(keyboardKeyStroke), List.of(keyboardKeyStrokeDelay, keyboardLayout)),
    keyboardType(CheckType.windowExists, List.of(keyboardTypeText), List.of(keyboardLayout)),
    killProcess(CheckType.processExists, List.of(), List.of()),
    delay(null, List.of(fixedDelayValue), List.of(randomDelayValue)) ;

    private final CheckType checkType;
    private final List<CommandDataType> mandatory;
    private final List<CommandDataType> optional;

    CommandType(final CheckType checkType, final List<CommandDataType> mandatory, final List<CommandDataType> optional) {
        this.checkType = checkType;
        this.mandatory = mandatory; //unmodifiable list
        this.optional = optional; //unmodifiable list
    }

    public Optional<CheckType> getCheckType() {
        return Optional.ofNullable(checkType);
    }

    public List<CommandDataType> mandatory() {
        return this.mandatory;
    }

    public List<CommandDataType> optional() {
        return this.optional;
    }

    private final static class Constants {
        public static final List<CommandDataType> MOUSE_CLICK_ABS_MANDATORY_COMMAND_DATA_TYPES = List.of(mouseAbsPosition);
        public static final List<CommandDataType> MOUSE_CLICK_MANDATORY_COMMAND_DATA_TYPES = List.of(mouseShotRelatedPosition);
        public static final List<CommandDataType> MOUSE_CLICK_OPTIONAL_COMMAND_DATA_TYPES = List.of(mousePositionOffset);
    }

}
