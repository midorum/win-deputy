package midorum.win32.deputy.i18n;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.BiFunction;

public enum UiElement {
    // ui elements
    newScenarioLabel,
    currentScenarioLabel,
    scenarioIsNotLoadedLabel,
    settingsLabel,
    aboutLabel,
    stampDeviationLabel,
    maxRepeatableCountLabel,
    initialDelayInSecondsLabel,
    delayBetweenRunsInSecondsLabel,
    userActivityDelayInSecondsLabel,
    listAllWindowsWhenSearchFailLabel,
    observeMouseButtonsLabel,
    rootLogLevelLabel,
    libLogLevelLabel,
    languageLabel,
    titleLabel,
    descriptionLabel,
    activityViewingLabel,
    activityEditingLabel,
    repeatableLabel,
    producesFragileStateLabel,
    checkLabel,
    ignoreLabel,
    commandLabel,
    timeoutInSecondsLabel,
    waitingListLabel,
    waitingLabel,
    enterDesiredKeystrokeTip,
    applyButtonText,
    closeButtonText,
    loadScenarioButtonText,
    editScenarioButtonText,
    createScenarioButtonText,
    runScenarioButtonText,
    stopScenarioButtonText,
    settingsButtonText,
    logsButtonText,
    aboutButtonText,
    cancelButtonText,
    saveButtonText,
    saveAsButtonText,
    saveAndCloseButtonText,
    // messages
    warningDialogTitle,
    windowDoesNotHaveTitle,
    workingDirectoryIsGoingToBeChangedWarning,
    noRectangleWasCaptured,
    askToReplaceExistingFile,
    scenarioExecutingInterrupted,
    scenarioDone,
    maxRepeatableCountExceeded,
    noOneActivityWasPerformed,
    scenarioSavedAsFileName,
    cannotDeleteLastActivity,
    cannotIgnoreLastActivity,
    cannotDeleteLastCheck,
    cannotIgnoreLastCheck,
    cannotDeleteLastCommand,
    cannotDeleteLastScenarioData,
    cannotDeleteLastCheckData,
    cannotDeleteLastCommandData,
    scenarioTitleCannotBeEmpty,
    scenarioTypeCannotBeEmpty,
    activityTitleCannotBeEmpty,
    illegalActivity,
    illegalActivities,
    illegalCheck,
    illegalChecks,
    illegalCommand,
    illegalCommands,
    illegalScenarioData,
    illegalCheckType,
    illegalCheckData,
    illegalCommandType,
    illegalCommandData,
    illegalWaiting,
    illegalWaitingList,
    illegalTimeoutValue,
    illegalTimeoutValue2,
    shouldFulfillActivityBeforeAdding,
    shouldFulfillActivityBeforeEditing,
    shouldFulfillActivityBeforeViewing,
    waitingShouldHaveDescription,
    keystrokeNotCaptured,
    // errors
    cannotLoadSettingsFromFile,
    cannotStoreSettingsToFile,
    errorOccurredWhileLoadingApplicationState,
    errorOccurredWhileSavingApplicationState,
    errorOccurredWhileLoadingScenario,
    errorOccurredWhileSavingScenario,
    errorOccurredWhileExecutingScenario,
    errorOccurredWhileSavingImage,
    cannotPickMousePointerPosition,
    cannotDetermineWindowByPoint,
    cannotGetProcessName,
    cannotGetProcessInformation,
    cannotGetWindowClassName,
    cannotGetWindowTitle,
    cannotGetWindowStyles,
    cannotGetWindowExtendedStyles,
    cannotOpenLogsLocation,
    cannotParseCoordinates,
    cannotGetShot,
    unsupportedCommandDataTypes,
    cannotGetDataType,
    cannotParseKeyboardLayout,
    cannotGetNativeWindow,
    cannotGetForegroundWindow;

    private final BiFunction<String, Object[], String> argumentMapper = (s, args) -> {
        if (args == null || args.length == 0) return s;
        String acc = s;
        for (final Object arg : args) {
            acc = acc.replaceFirst("\\{}", Objects.toString(arg));
        }
        return acc;
    };

    public String forDefaultLocale(final Object... args) {
        return I18nResourcesProvider.getInstance().forDefaultLocale(this.name())
                .map(s -> argumentMapper.apply(s, args))
                .orElse(this.name());
    }

    public String forUserLocale(final Object... args) {
        return I18nResourcesProvider.getInstance().forUserLocale(this.name())
                .map(s -> argumentMapper.apply(s, args))
                .orElse(this.name());
    }

}
