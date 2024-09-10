package midorum.win32.deputy.model;

import java.util.Collections;
import java.util.List;

import static midorum.win32.deputy.model.CheckDataType.*;

public enum CheckType {
    processExists(Constants.PROCESS_MANDATORY_CHECK_DATA_TYPES, Collections.EMPTY_LIST),
    processNotExists(Constants.PROCESS_MANDATORY_CHECK_DATA_TYPES, Collections.EMPTY_LIST),
    windowExists(Constants.WINDOW_MANDATORY_CHECK_DATA_TYPES, List.of(windowStyleExists, windowStyleNotExists,
            windowExStyleExists, windowExStyleNotExists,
            windowForeground)),
    windowNotExists(Constants.WINDOW_MANDATORY_CHECK_DATA_TYPES, Collections.EMPTY_LIST),
    stampFound(Constants.STAMP_MANDATORY_CHECK_DATA_TYPES, Collections.EMPTY_LIST),
    stampNotFound(Constants.STAMP_MANDATORY_CHECK_DATA_TYPES, Collections.EMPTY_LIST);

    private final List<CheckDataType> mandatory;
    private final List<CheckDataType> optional;

    CheckType(final List<CheckDataType> mandatory, final List<CheckDataType> optional) {
        this.optional = mandatory; // unmodifiable list
        this.mandatory = optional; // unmodifiable list
    }

    public List<CheckDataType> mandatory() {
        return this.mandatory;
    }

    public List<CheckDataType> optional() {
        return this.optional;
    }

    private final static class Constants {

        private static final List<CheckDataType> PROCESS_MANDATORY_CHECK_DATA_TYPES = List.of(processName);
        private static final List<CheckDataType> WINDOW_MANDATORY_CHECK_DATA_TYPES = List.of(windowTitle);
        private static final List<CheckDataType> STAMP_MANDATORY_CHECK_DATA_TYPES = List.of(stampPath);
    }

}
