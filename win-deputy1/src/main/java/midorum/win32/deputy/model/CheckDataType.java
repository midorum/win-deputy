package midorum.win32.deputy.model;


import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public enum CheckDataType {
    processName(List.of(SourceType.userInput, SourceType.pickWindowProcessName)),
    processExistsGreaterThan(List.of(SourceType.positiveIntegerInput), TimeUnit.SECONDS),
    processExistsLessThan(List.of(SourceType.positiveIntegerInput), TimeUnit.SECONDS),
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
    private final TimeUnit unit;

    CheckDataType(final List<SourceType> sourceTypes, final TimeUnit unit) {
        this.sourceTypes = sourceTypes;
        this.unit = unit;
    }

    CheckDataType(final List<SourceType> sourceTypes) {
        this(sourceTypes, null);
    }

    public List<SourceType> getSourceTypes() {
        return sourceTypes;
    }

    public Optional<TimeUnit> getUnit() {
        return Optional.ofNullable(unit);
    }
}
