package midorum.win32.deputy.model;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public enum CommandDataType {
    mouseAbsPosition(List.of(SourceType.coordinatesInput, SourceType.pickAbsolutePoint)),
    mouseShotRelatedPosition(List.of(SourceType.choseCheckId)),
    mousePositionOffset(List.of(SourceType.coordinatesOffsetInput, SourceType.pickPointOffset)),
    keyboardTypeText(List.of(SourceType.userInput)),
    keyboardLayout(List.of(SourceType.keyboardLayoutChoice)),
    keyboardKeyStroke(List.of(SourceType.userInput, SourceType.captureKeystroke)),
    keyboardKeyStrokeDelay(List.of(SourceType.positiveIntegerInput), TimeUnit.MILLISECONDS),
    fixedDelayValue(List.of(SourceType.positiveIntegerInput), TimeUnit.MILLISECONDS),
    randomDelayValue(List.of(SourceType.positiveIntegerInput), TimeUnit.MILLISECONDS);

    private final List<SourceType> sourceTypes;
    private final TimeUnit unit;

    CommandDataType(final List<SourceType> sourceTypes, final TimeUnit unit) {
        this.sourceTypes = sourceTypes;
        this.unit = unit;
    }

    CommandDataType(final List<SourceType> sourceTypes) {
        this(sourceTypes, null);
    }

    public List<SourceType> getSourceTypes() {
        return sourceTypes;
    }

    public Optional<TimeUnit> getUnit() {
        return Optional.ofNullable(unit);
    }
}
