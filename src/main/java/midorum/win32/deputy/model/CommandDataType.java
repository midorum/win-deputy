package midorum.win32.deputy.model;

import java.util.List;

public enum CommandDataType {
    mousePosition(List.of(SourceType.coordinatesInput, SourceType.pickAbsolutePoint)),
    mouseShotRelatedPosition(List.of(SourceType.pickFile, SourceType.makeShot)), // todo add types for pick relative point
    keyboardTypeText(List.of(SourceType.userInput)),
    keyboardLayout(List.of(SourceType.keyboardLayoutChoice)),
    keyboardKeyCode(List.of(SourceType.userInput)),
    keyboardAltFlag(List.of(SourceType.booleanChoice)),
    keyboardCtrlFlag(List.of(SourceType.booleanChoice)),
    keyboardShiftFlag(List.of(SourceType.booleanChoice));

    private final List<SourceType> sourceTypes;

    CommandDataType(final List<SourceType> sourceTypes) {
        this.sourceTypes = sourceTypes;
    }

    public List<SourceType> getSourceTypes() {
        return sourceTypes;
    }

}
