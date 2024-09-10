package midorum.win32.deputy.model;

public enum ScenarioType {
    /**
     * повторяется, пока не быдет выполнена последняя активность сценария, после этого останавливается
     */
    oneTime, 
    /**
     * повторяется бесконечно
     */
    repeatable
}
