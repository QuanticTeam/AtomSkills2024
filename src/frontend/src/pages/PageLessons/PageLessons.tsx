import { useTranslation } from 'react-i18next'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { Lessons } from '~/widgets'

export function PageLessons() {
  const { t } = useTranslation(PageLessons.name)

  return (
    <PageAuthorized title={t('title')}>
      <Lessons />
    </PageAuthorized>
  )
}
