package midorum.win32.deputy.model;

/// import am.ik.yavi.builder.ValidatorBuilder;
/// import am.ik.yavi.core.Constraint;
/// import am.ik.yavi.core.Validator;

import java.util.Map;


public class Check {

    ///    public static final Validator<Check> validator = ValidatorBuilder.<Check>of()
    ///            .constraint(Check::getType, "check type", Constraint::notNull)
    ///            .constraint(Check::getData, "check data", c -> c.notNull().notEmpty())
    ///            .constraintOnCondition((check, constraintContext) -> check.getType() == CheckType.processExists,
    ///                    b -> b.constraint(Check::getData, "check data",
    ///                            c -> c.predicate(dataMap -> CheckType.processExists.mandatory().stream()
    ///                                            .anyMatch(dataMap::containsKey),
    ///                                    "test 1", "\"{0}\" should contain all of " + CheckType.processExists.mandatory() + " but contains only: \"{1}\" ")))
    ///            .build();

    private final CheckType type;
    private final Map<CheckDataType, String> data;
    private final boolean ignore;

    public Check(CheckType type, Map<CheckDataType, String> data, final boolean ignore) {
        this.type = type;
        this.data = data;
        this.ignore = ignore;
    }

    public Check() {
        this.type = null;
        this.data = null;
        this.ignore = false;
    }

    public static boolean validateCheckType(final CheckType checkType) {
        return checkType != null;
    }

    public static boolean validateCheckDataEntry(final CheckDataType checkDataType, final String checkDataValue) {
        if (checkDataType == null) return false;
        return checkDataValue != null && !checkDataValue.isBlank();
    }

    public static boolean validateCheckData(final CheckType checkType, final Map<CheckDataType, String> data) {
        return checkType.mandatory().stream().anyMatch(data::containsKey);
    }

    public CheckType getType() {
        return this.type;
    }

    public Map<CheckDataType, String> getData() {
        return this.data;
    }

    public boolean isIgnore() {
        return ignore;
    }

    @Override
    public String toString() {
        return "Check{" +
                "type=" + type +
                ", data=" + data +
                ", ignore=" + ignore +
                '}';
    }
}
