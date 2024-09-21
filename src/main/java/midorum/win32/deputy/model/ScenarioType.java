package midorum.win32.deputy.model;

import java.util.List;

public enum ScenarioType {
    /**
     * Повторяется, пока не будет выполнена последняя активность сценария, после этого останавливается
     */
    oneTime(List.of(), List.of()),
    /**
     * Повторяется бесконечно
     */
    repeatable(List.of(), List.of(ScenarioDataType.repeatDelay));

    private final List<ScenarioDataType> mandatory;
    private final List<ScenarioDataType> optional;

    ScenarioType(final List<ScenarioDataType> mandatory, final List<ScenarioDataType> optional) {
        this.mandatory = mandatory;
        this.optional = optional;
    }

    public List<ScenarioDataType> mandatory() {
        return mandatory;
    }

    public List<ScenarioDataType> optional() {
        return optional;
    }
}
