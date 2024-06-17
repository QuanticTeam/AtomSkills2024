import i18next from 'i18next'
import { Lessons } from './Lessons'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', Lessons.name, en)
i18next.addResourceBundle('ru', Lessons.name, ru)

export * from './Lessons'
