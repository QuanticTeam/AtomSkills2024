import i18next from 'i18next'
import { PageTask } from './PageTask'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageTask.name, en)
i18next.addResourceBundle('ru', PageTask.name, ru)

export default PageTask
