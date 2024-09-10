package midorum.win32.deputy.model;

import java.util.List;
import java.util.Map;

/**
 * Именованный список активностей
 * 
 * @author midorum
 */
public class Scenario {

    private final String title;
    private final String description;
    private final List<Activity> activities;
    private final ScenarioType type;
    private final Map<ScenarioDataType, String> data;

    public Scenario(String title, String description, List<Activity> activities, ScenarioType type, Map<ScenarioDataType, String> data) {
        this.title = title;
        this.description = description;
        this.activities = activities;
        this.type = type;
        this.data = data;
    }

    public String getTitle() {
        return this.title;
    }

    public String getDescription() {
        return this.description;
    }

    public List<Activity> getActivities() {
        return this.activities;
    }

    public ScenarioType getType() {
        return this.type;
    }

    public Map<ScenarioDataType,String> getData() {
        return this.data;
    }

    @Override
    public String toString() {
        return "{" +
            " title='" + getTitle() + "'" +
            ", description='" + getDescription() + "'" +
            ", activities='" + getActivities() + "'" +
            ", type='" + getType() + "'" +
            ", data='" + getData() + "'" +
            "}";
    }

}
