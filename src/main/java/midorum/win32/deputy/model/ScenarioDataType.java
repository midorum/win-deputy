package midorum.win32.deputy.model;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public enum ScenarioDataType {
    /**
     * задержка между повторениями сценария
     */
    repeatDelay(List.of(SourceType.positiveIntegerInput), TimeUnit.SECONDS);

    private final List<SourceType> sourceTypes;
    private final TimeUnit unit;

    ScenarioDataType(final List<SourceType> sourceTypes, final TimeUnit unit) {
        this.sourceTypes = sourceTypes;
        this.unit = unit;
    }

    public List<SourceType> getSourceTypes() {
        return sourceTypes;
    }

    public Optional<TimeUnit> getUnit() {
        return Optional.ofNullable(unit);
    }
}
