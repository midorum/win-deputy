package midorum.win32.deputy.ui;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import com.midorum.win32api.facade.IProcess;
import com.midorum.win32api.facade.IWindow;
import com.midorum.win32api.facade.Win32System;
import com.midorum.win32api.hook.MouseHookHelper;
import com.midorum.win32api.struct.PointInt;
import com.midorum.win32api.win32.IWinUser;
import midorum.win32.deputy.model.Scenario;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

class Win32Utilities {

    private final Logger logger = LogManager.getLogger(this);
    private final Component parentComponent;

    public Win32Utilities(final Component parentComponent) {
        this.parentComponent = parentComponent;
    }

    public void captureAndSaveRegion(final State state,
                                     final Consumer<File> resultConsumer) {
        new CaptureWindow(maybeImage -> maybeImage.ifPresentOrElse(image -> Util.askNewFileAndUpdateState(state, file ->
                        Util.saveImage(image, Util.getPathForImages(state.getWorkingDirectory()), file.getName())
                                .consumeOrHandleError(resultConsumer,
                                        e -> reportThrowable("Error occurred while saving image", e)), parentComponent),
                () -> reportState("No rectangle was captured"))).display();
    }

    public void pickFile(final State state, final Consumer<File> successFileConsumer) {
        final BiConsumer<File, File> updateStateAndDoJob = (workingDirectory, acceptedFile) -> {
            state.setWorkingDirectory(workingDirectory);
            successFileConsumer.accept(acceptedFile);
        };
        Util.askExistFile(state.getWorkingDirectory(), parentComponent).ifPresent(file -> {
            final File workingDirectory = state.getWorkingDirectory();
            final File selectedWorkingDirectory = Util.getWorkingDirectoryForPath(file.getParentFile());
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
        pickAbsolutePoint(pointInt -> Win32System.getInstance().getWindowByPoint(pointInt)
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

    private void reportThrowable(final String message, final Throwable t) {
        logger.error(message, t);
        JOptionPane.showMessageDialog(parentComponent, message);
    }

    private void reportIllegalState(final String message) {
        logger.error(message);
        JOptionPane.showMessageDialog(parentComponent, message);
    }

    private void reportState(final String message) {
        JOptionPane.showMessageDialog(parentComponent, message);
    }

}
