import i18next from 'i18next'
import { Attachments } from './Attachments'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', Attachments.name, en)
i18next.addResourceBundle('ru', Attachments.name, ru)

export * from './Attachments'
