package midorum.win32.deputy.model;

public enum ScenarioType {
    /**
     * Повторяется, пока не будет выполнена последняя активность сценария, после этого останавливается
     */
    oneTime, 
    /**
     * Повторяется бесконечно
     */
    repeatable
}
