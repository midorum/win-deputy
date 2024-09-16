package midorum.win32.deputy.ui;

import dma.file.FileInputStream;
import dma.file.FileOutputStream;
import midorum.win32.deputy.common.Either;

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

    public static Either<State, IOException> loadState() {
        return Either.value(() -> FileInputStream.getInstance().useWithInputStream(STATE_FILE_NAME, inputStream -> {
            Properties appProps = new Properties();
            appProps.load(inputStream);
            final State state = new State();
            state.setWorkingDirectory(new File(appProps.getProperty(WORKING_DIRECTORY_PROPERTY_NAME)));
            state.setScenarioName(appProps.getProperty(SCENARIO_NAME_PROPERTY_NAME));
            return state;
        })).orException();
    }

    public void storeToFile() throws IOException {
        FileOutputStream.getInstance().rewriteWithOutputStream(STATE_FILE_NAME, outputStream -> {
            Properties appProps = new Properties();
            appProps.setProperty(WORKING_DIRECTORY_PROPERTY_NAME, getWorkingDirectory().getAbsolutePath());
            appProps.setProperty(SCENARIO_NAME_PROPERTY_NAME, getScenarioName());
            appProps.store(outputStream, null);
        });
    }
}
