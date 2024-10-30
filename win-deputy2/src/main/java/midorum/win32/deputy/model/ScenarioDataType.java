package midorum.win32.deputy.model;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public enum ScenarioDataType {
    /**
     * Задержка между повторениями сценария
     */
    repeatDelay(List.of(SourceType.positiveIntegerInput), TimeUnit.SECONDS),
    /**
     * Задержка между повторениями сценария с продолжительностью в пределах от 0 до указанного значения
     */
    randomDelay(List.of(SourceType.positiveIntegerInput), TimeUnit.SECONDS);

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
