package midorum.win32.deputy.model;


import java.util.List;

public enum CheckDataType {
    processName(List.of(SourceType.userInput)),
    windowTitle(List.of(SourceType.userInput)),
    windowStyleExists(List.of(SourceType.userInput)),
    windowStyleNotExists(List.of(SourceType.userInput)),
    windowExStyleExists(List.of(SourceType.userInput)),
    windowExStyleNotExists(List.of(SourceType.userInput)),
    windowForeground(List.of(SourceType.userInput)),
    stampPath(List.of(SourceType.pickFile, SourceType.makeShot));

    private final List<SourceType> sourceTypes;

    CheckDataType(final List<SourceType> sourceTypes) {
        this.sourceTypes = sourceTypes;
    }

    public List<SourceType> getSourceTypes() {
        return sourceTypes;
    }
}
