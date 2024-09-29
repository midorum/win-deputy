package midorum.win32.deputy.ui;

import com.midorum.win32api.facade.IProcess;
import com.midorum.win32api.facade.IWindow;
import com.midorum.win32api.hook.MouseHookHelper;
import com.midorum.win32api.struct.PointInt;
import com.midorum.win32api.win32.IWinUser;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.common.Win32Adapter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

class UiUtil {

    private final Logger logger = LogManager.getLogger("ui");
    private final Component parentComponent;
    private final Win32Adapter win32Adapter;

    public UiUtil(final Component parentComponent, final Win32Adapter win32Adapter) {
        this.parentComponent = parentComponent;
        this.win32Adapter = win32Adapter;
    }

    public Logger getLogger() {
        return logger;
    }

    public Win32Adapter getWin32Adapter() {
        return win32Adapter;
    }

    public Optional<File> askFile(final File currentDirectory, final boolean shouldExist) {
        final JFileChooser fileChooser = new JFileChooser();
        fileChooser.setCurrentDirectory(currentDirectory);
        if (fileChooser.showOpenDialog(parentComponent) == JFileChooser.APPROVE_OPTION) {
            final File selectedFile = fileChooser.getSelectedFile();
            if (shouldExist) {
                if (selectedFile.exists()) return Optional.of(selectedFile);
            } else {
                if (!selectedFile.exists()
                        || JOptionPane.showConfirmDialog(parentComponent,
                        "Do you want to replace existing file?",
                        "Warning",
                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    return Optional.of(new File(CommonUtil.getWorkingDirectoryForPath(selectedFile.getParentFile()), selectedFile.getName()));
                } else {
                    return Optional.of(new File(CommonUtil.getWorkingDirectoryForPath(selectedFile.getParentFile()), CommonUtil.getDefaultFileName()));
                }
            }
        }
        return Optional.empty();
    }

    public Optional<File> askNewFile(final File currentDirectory) {
        return askFile(currentDirectory, false);
    }

    public Optional<File> askExistFile(final File currentDirectory) {
        return askFile(currentDirectory, true);
    }

    public void askNewFileAndUpdateState(final State state, final Consumer<File> fileConsumer) {
        askNewFile(state.getWorkingDirectory()).ifPresent(file -> {
            final File workingDirectory = state.getWorkingDirectory();
            final File selectedDirectory = file.getParentFile();
            if (workingDirectory == null) {
                state.setWorkingDirectory(selectedDirectory);
            } else if (!Objects.equals(workingDirectory, selectedDirectory)) {
                if (JOptionPane.showConfirmDialog(parentComponent,
                        "Your selected directory does not match current working directory." +
                                " It is not recommended to change working directory." +
                                " Would you like to change working directory anyway?",
                        "Warning",
                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    state.setWorkingDirectory(selectedDirectory);
                }
            }
            fileConsumer.accept(file);
        });
    }

    public void captureAndSaveRegion(final State state,
                                     final Consumer<File> resultConsumer) {
        new CaptureWindow(maybeImage -> maybeImage.ifPresentOrElse(image -> askNewFileAndUpdateState(state, file ->
                        CommonUtil.saveImage(image, CommonUtil.getPathForImages(state.getWorkingDirectory()), file.getName())
                                .consumeOrHandleError(resultConsumer,
                                        e -> reportThrowable("Error occurred while saving image", e))),
                () -> reportState("No rectangle was captured")), win32Adapter).display();
    }

    public void pickFile(final State state, final Consumer<File> successFileConsumer) {
        final BiConsumer<File, File> updateStateAndDoJob = (workingDirectory, acceptedFile) -> {
            state.setWorkingDirectory(workingDirectory);
            successFileConsumer.accept(acceptedFile);
        };
        askExistFile(state.getWorkingDirectory()).ifPresent(file -> {
            final File workingDirectory = state.getWorkingDirectory();
            final File selectedWorkingDirectory = CommonUtil.getWorkingDirectoryForPath(file.getParentFile());
            if (workingDirectory == null) {
                updateStateAndDoJob.accept(selectedWorkingDirectory, file);
                return;
            }
            if (Objects.equals(workingDirectory, selectedWorkingDirectory)) {
                successFileConsumer.accept(file);
            } else if (JOptionPane.showConfirmDialog(parentComponent,
                    "Your selected directory does not match current working directory." +
                            " It is not recommended to change working directory." +
                            " Would you like to change working directory anyway?",
                    "Warning",
                    JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                updateStateAndDoJob.accept(selectedWorkingDirectory, file);
            }
        });
    }

    public void pickAbsolutePoint(final Consumer<PointInt> resultConsumer) {
        MouseHookHelper.getInstance().setGlobalHookForKey(IWinUser.WM_LBUTTONDOWN,
                (mouseEvent) -> {
                    resultConsumer.accept(mouseEvent.point());
                    return true;
                },
                throwable -> {
                    reportThrowable("Cannot pick mouse pointer position", throwable);
                    return true;
                });
    }

    private void pickWindowByPoint(final Consumer<IWindow> resultConsumer) {
        pickAbsolutePoint(pointInt -> win32Adapter.getWindowByPoint(pointInt)
                .ifPresentOrElse(windowPoint -> resultConsumer.accept(windowPoint.window()),
                        () -> reportIllegalState("Cannot determine window by point: " + pointInt)));
    }

    public void pickWindowProcessName(final Consumer<String> resultConsumer) {
        pickWindowByPoint(window -> window.getProcess()
                .consumeOrHandleError((Consumer<? super IProcess>) iProcess -> iProcess.name()
                                .ifPresentOrElse(resultConsumer,
                                        () -> reportIllegalState("Cannot obtain process name")),
                        e -> reportThrowable("Cannot obtain process information", e)));
    }

    public void pickWindowClassName(final Consumer<String> resultConsumer) {
        pickWindowByPoint(window -> window.getClassName().consumeOrHandleError(resultConsumer,
                e -> reportThrowable("Cannot obtain window class name", e)));
    }

    public void pickWindowTitle(final Consumer<String> resultConsumer) {
        pickWindowByPoint(window -> window.getText()
                .consumeOrHandleError((Consumer<? super Optional<String>>) s -> s.ifPresentOrElse(resultConsumer,
                                () -> reportState("Window doesn't have title")),
                        e -> reportThrowable("Cannot get window title", e)));
    }

    public void pickWindowStyles(final Consumer<String> resultConsumer) {
        pickWindowByPoint(window -> window.getStyle().consumeOrHandleError((Consumer<? super Integer>) styles ->
                        resultConsumer.accept(Integer.toString(styles)),
                e -> reportThrowable("Cannot get window styles", e)));
    }

    public void pickWindowExStyles(final Consumer<String> resultConsumer) {
        pickWindowByPoint(window -> window.getExtendedStyle().consumeOrHandleError((Consumer<? super Integer>) styles ->
                        resultConsumer.accept(Integer.toString(styles)),
                e -> reportThrowable("Cannot get window extended styles", e)));
    }

    public void reportThrowable(final String message, final Throwable t) {
        logger.error(message, t);
        JOptionPane.showMessageDialog(parentComponent, message);
    }

    public void reportIllegalState(final String message) {
        logger.error(message);
        JOptionPane.showMessageDialog(parentComponent, message);
    }

    public void reportState(final String message) {
        JOptionPane.showMessageDialog(parentComponent, message);
    }

    public void logThrowable(final String message, final Throwable t) {
        logger.error(message, t);
    }

    public void logIllegalState(final String message) {
        logger.error(message);
    }

}
