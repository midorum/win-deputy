package midorum.win32.deputy.ui;

import dma.file.v2.FileUtil;
import dma.flow.Either;
import midorum.win32.deputy.common.Win32Adapter;

import java.io.File;
import java.io.IOException;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

class State {
    public static final String STATE_FILE_NAME = "state";
    public static final String WORKING_DIRECTORY_PROPERTY_NAME = "workingDirectory";
    public static final String SCENARIO_NAME_PROPERTY_NAME = "scenarioName";
    private final AtomicReference<File> workingDirectory = new AtomicReference<>();
    private final AtomicReference<String> scenarioName = new AtomicReference<>();
    private final UiUtil utilities;

    State(final UiUtil utilities) {
        this.utilities = utilities;
    }

    public UiUtil getUtilities() {
        return utilities;
    }

    public Win32Adapter getWin32Adapter() {
        return utilities.getWin32Adapter();
    }

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

    public static Either<State, IOException> loadState(final UiUtil utilities) {
        return Either.valueFrom(() -> FileUtil.withText(STATE_FILE_NAME).asReader(reader -> {
            Properties appProps = new Properties();
            appProps.load(reader);
            final State state = new State(utilities);
            state.setWorkingDirectory(new File(appProps.getProperty(WORKING_DIRECTORY_PROPERTY_NAME)));
            state.setScenarioName(appProps.getProperty(SCENARIO_NAME_PROPERTY_NAME));
            return state;
        })).orException();
    }

    public void storeToFile() throws IOException {
        FileUtil.withText(STATE_FILE_NAME).overwriteThroughWriter(writer -> {
            Properties appProps = new Properties();
            appProps.setProperty(WORKING_DIRECTORY_PROPERTY_NAME, getWorkingDirectory().getAbsolutePath());
            appProps.setProperty(SCENARIO_NAME_PROPERTY_NAME, getScenarioName());
            appProps.store(writer, null);
        });
    }
}
