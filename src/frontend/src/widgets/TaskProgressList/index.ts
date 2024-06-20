import i18next from 'i18next'
import { TaskProgressList } from './TaskProgressList'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', TaskProgressList.name, en)
i18next.addResourceBundle('ru', TaskProgressList.name, ru)

export * from './TaskProgressList'
