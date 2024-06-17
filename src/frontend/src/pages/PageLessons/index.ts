import i18next from 'i18next'
import { PageLessons } from './PageLessons'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageLessons.name, en)
i18next.addResourceBundle('ru', PageLessons.name, ru)

export default PageLessons
