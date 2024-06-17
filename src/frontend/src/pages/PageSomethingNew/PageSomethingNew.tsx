import { SubmitHandler } from 'react-hook-form'
import { useTranslation } from 'react-i18next'
import { useNavigate } from 'react-router-dom'
import { SomethingNewDto } from '~/entities/Something'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { apiClient } from '~/shared/apiClient'
import { ROUTE_PATH_SOMETHING } from '~/shared/routing'
import { FormSomethingNew } from '~/widgets'

export function PageSomethingNew() {
  const { t } = useTranslation(PageSomethingNew.name)
  const navigate = useNavigate()

  const onSubmit: SubmitHandler<SomethingNewDto> = async dto => {
    await apiClient.post('/Something/Create', dto)
  }

  return (
    <PageAuthorized title={t('title')}>
      <FormSomethingNew
        onSubmit={async (...args) => {
          await onSubmit(...args) // TODO handle errors
          navigate(ROUTE_PATH_SOMETHING)
        }}
        children={null}
      />
    </PageAuthorized>
  )
}
