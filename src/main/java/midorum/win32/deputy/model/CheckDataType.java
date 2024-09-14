package midorum.win32.deputy.model;


import java.util.List;

public enum CheckDataType {
    processName(List.of(SourceType.userInput, SourceType.pickWindowProcessName)),
    windowTitle(List.of(SourceType.userInput, SourceType.pickWindowTitle)),
    windowClassName(List.of(SourceType.userInput, SourceType.pickWindowClassName)),
    windowStyleIs(List.of(SourceType.userInput, SourceType.pickWindowStyles)),
    windowExStyleIs(List.of(SourceType.userInput, SourceType.pickWindowExStyles)),
    windowStyleExists(List.of(SourceType.windowStyleChoice)),
    windowStyleNotExists(List.of(SourceType.windowStyleChoice)),
    windowExStyleExists(List.of(SourceType.windowExStyleChoice)),
    windowExStyleNotExists(List.of(SourceType.windowExStyleChoice)),
    windowForeground(List.of(SourceType.booleanChoice)),
    stampPath(List.of(SourceType.pickFile, SourceType.makeShot));

    private final List<SourceType> sourceTypes;

    CheckDataType(final List<SourceType> sourceTypes) {
        this.sourceTypes = sourceTypes;
    }

    public List<SourceType> getSourceTypes() {
        return sourceTypes;
    }

}
