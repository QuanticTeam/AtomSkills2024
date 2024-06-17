import i18next from 'i18next'
import { PageAuthorized } from './PageAuthorized'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageAuthorized.name, en)
i18next.addResourceBundle('ru', PageAuthorized.name, ru)

export * from './PageAuthorized'
