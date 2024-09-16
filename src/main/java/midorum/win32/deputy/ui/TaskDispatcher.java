package midorum.win32.deputy.ui;

public interface TaskDispatcher {
    void createNewScenario();
    void cancelScenarioEditing();
    void endScenarioEditing(String scenarioPath);
    void editScenario(String scenarioPath);
    void loadScenario();
}
