import i18next from 'i18next'
import { Tasks } from './Tasks'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', Tasks.name, en)
i18next.addResourceBundle('ru', Tasks.name, ru)

export * from './Tasks'
