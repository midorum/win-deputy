package midorum.win32.deputy.model;

public enum SourceType {
    /**
     * если не указан этот тип, то пользовательский ввод запрещен (вместо поля для ввода должна быть отрисована метка с содержимым значения типа
     */
    userInput,
    /**
     * если указан этот тип, то должна быть нарисована кнопка для выбора файла; возвращаемый тип - путь к файлу
     */
    pickFile,
    /**
     * если указан этот тип, то должна быть нарисована кнопка для взятия скриншота региона; возвращаемый тип - путь к файлу скриншота
     */
    makeShot,
    pickWindowProcessName,
    pickWindowTitle,
    pickWindowStyles,
    pickWindowExStyles,
    booleanChoice,
    coordinatesInput,
    pickCoordinates,
    keyboardLayoutChoice
}
