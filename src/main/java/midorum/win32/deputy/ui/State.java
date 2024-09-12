package midorum.win32.deputy.ui;

import java.io.File;
import java.util.concurrent.atomic.AtomicReference;

class State {
    private final AtomicReference<File> workingDirectory = new AtomicReference<>();
    private final AtomicReference<String> scenarioName = new AtomicReference<>();

    public File getWorkingDirectory() {
        return workingDirectory.get();
    }

    public void setWorkingDirectory(final File workingDirectory) {
        this.workingDirectory.set(workingDirectory);
    }

    public String getScenarioName() {
        return scenarioName.get();
    }

    public void setScenarioName(final String scenarioName) {
        this.scenarioName.set(scenarioName);
    }
}
