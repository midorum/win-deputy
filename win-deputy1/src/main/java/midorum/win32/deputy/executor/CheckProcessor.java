package midorum.win32.deputy.executor;

import com.midorum.win32api.facade.IProcess;
import com.midorum.win32api.facade.IWindow;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;

public class CheckProcessor {

    private final Logger logger = LogManager.getLogger(IExecutor.LOGGER_NAME);

    private final Win32Cache cache;

    public CheckProcessor(final Win32Cache cache) {
        this.cache = cache;
    }

    public boolean verifyCheck(final Check check, final boolean beGentle) {
        return switch (check.getType()) {
            case processExists -> getProcess(check.getData()).isPresent();
            case processNotExists -> getProcess(check.getData()).isEmpty();
            case windowExists -> getWindow(check.getData()).isPresent();
            case windowNotExists -> getWindow(check.getData()).isEmpty();
            case stampFound -> getStamp(check.getData(), beGentle).isPresent();
            case stampNotFound -> getStamp(check.getData(), beGentle).isEmpty();
        };
    }

    private Optional<IProcess> getProcess(final Map<CheckDataType, String> data) {
        final String processName = data.get(CheckDataType.processName);
        if (processName == null || processName.isBlank())
            throw new IllegalArgumentException("Cannot obtain process name from check data");
        final String processExistsGreaterThanData = data.get(CheckDataType.processExistsGreaterThan);
        final String processExistsLessThanData = data.get(CheckDataType.processExistsLessThan);
        if (processExistsGreaterThanData == null && processExistsLessThanData == null)
            return cache.getProcess(processName);
        return cache.getProcess(processName,
                processExistsGreaterThanData != null ? Long.valueOf(processExistsGreaterThanData) : null,
                processExistsLessThanData != null ? Long.valueOf(processExistsLessThanData) : null);
    }

    private Optional<IWindow> getWindow(final Map<CheckDataType, String> data) {
        final String windowTitle = data.get(CheckDataType.windowTitle);
        if (windowTitle == null || windowTitle.isBlank())
            throw new IllegalArgumentException("Cannot obtain window title from check data");
        final String windowClassName = data.get(CheckDataType.windowClassName);
        return cache.getWindow(windowTitle, windowClassName)
                .map(window -> {
                    final String windowForegroundValue = data.get(CheckDataType.windowForeground);
                    if (windowForegroundValue != null) {
                        if (Boolean.parseBoolean(windowForegroundValue) != window.isForeground()) {
                            return null;
                        }
                    }
                    final String windowStyleIsValue = data.get(CheckDataType.windowStyleIs);
                    if (windowStyleIsValue != null) {
                        if (Integer.parseInt(windowStyleIsValue) != window.getStyle().getOrHandleError(e -> {
                            logger.error("error occurred while obtaining window style", e);
                            return -1;
                        })) {
                            return null;
                        }
                    }
                    final String windowExStyleIsValue = data.get(CheckDataType.windowExStyleIs);
                    if (windowExStyleIsValue != null) {
                        if (Integer.parseInt(windowExStyleIsValue) != window.getExtendedStyle().getOrHandleError(e -> {
                            logger.error("error occurred while obtaining window extended style", e);
                            return -1;
                        })) {
                            return null;
                        }
                    }
                    final String windowStyleExistsValue = data.get(CheckDataType.windowStyleExists);
                    if (windowStyleExistsValue != null) {
                        throw new UnsupportedOperationException("windowStyleExists processing is not supported yet");
                    }
                    final String windowStyleNotExistsValue = data.get(CheckDataType.windowStyleNotExists);
                    if (windowStyleNotExistsValue != null) {
                        throw new UnsupportedOperationException("windowStyleNotExists processing is not supported yet");
                    }
                    final String windowExStyleExistsValue = data.get(CheckDataType.windowExStyleExists);
                    if (windowExStyleExistsValue != null) {
                        throw new UnsupportedOperationException("windowExStyleExists processing is not supported yet");
                    }
                    final String windowExStyleNotExistsValue = data.get(CheckDataType.windowExStyleNotExists);
                    if (windowExStyleNotExistsValue != null) {
                        throw new UnsupportedOperationException("windowExStyleNotExists processing is not supported yet");
                    }
                    return window;
                });
    }

    private Optional<Stamp> getStamp(final Map<CheckDataType, String> data, final boolean subtleCheck) {
        final String stampPath = data.get(CheckDataType.stampPath);
        if (stampPath == null || stampPath.isBlank())
            throw new IllegalArgumentException("Cannot obtain stamp path from check data");
        return cache.getStamp(stampPath, subtleCheck);
    }

}
