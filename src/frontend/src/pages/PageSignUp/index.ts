import i18next from 'i18next'
import { PageSignUp } from './PageSignUp'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageSignUp.name, en)
i18next.addResourceBundle('ru', PageSignUp.name, ru)

export default PageSignUp
