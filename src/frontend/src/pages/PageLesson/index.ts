import i18next from 'i18next'
import { PageLesson } from './PageLesson'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageLesson.name, en)
i18next.addResourceBundle('ru', PageLesson.name, ru)

export default PageLesson
