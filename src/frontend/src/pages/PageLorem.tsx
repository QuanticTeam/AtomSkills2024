import { faker } from '@faker-js/faker'
import { App, Button } from 'antd'
import { AxiosError } from 'axios'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { apiClient } from '~/shared/apiClient'

export default function PageLorem() {
  const { notification } = App.useApp()

  return (
    <PageAuthorized title="Page title">
      <div>
        <p>
          <Button
            type="primary"
            onClick={async () => {
              try {
                const result = await apiClient.post(`/Something/Test`)

                notification.success({
                  message: 'Request OK',
                  description: result.data,
                })
              } catch (e) {
                if (e instanceof AxiosError) {
                  notification.error({
                    message: e.message,
                    description: <code className="text-xs">{JSON.stringify(e.response)}</code>,
                  })
                }
              }
            }}
          >
            Backend test request
          </Button>
        </p>

        <br />

        <p>{faker.lorem.sentence(1000)}</p>
      </div>
    </PageAuthorized>
  )
}
