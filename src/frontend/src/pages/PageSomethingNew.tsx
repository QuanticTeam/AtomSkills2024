import { zodResolver } from '@hookform/resolvers/zod'
import { Alert, BreadcrumbProps, Button, Form, Input, Popover, Typography, message } from 'antd'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { NavLink, useNavigate } from 'react-router-dom'
import { z } from 'zod'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { apiClient } from '~/shared/apiClient'

const breadcrumbs: BreadcrumbProps['items'] = [
  {
    title: (
      <NavLink
        to="/something"
        className={({ isActive }) => (!isActive ? '!text-link-1' : '')}
        end
      >
        Все сущности
      </NavLink>
    ),
  },
  {
    title: (
      <NavLink
        to="/something/new"
        className={({ isActive }) => (!isActive ? '!text-link-1' : '')}
        end
      >
        Новая сущность
      </NavLink>
    ),
  },
]

const title = 'Новая сущность'

const schema = z.object({
  name: z.string(),
  number: z.string(), // parsefloat
  integer: z.string(), // parseint
  dateTime: z.string(), // ISO string validation
  file: z.string(), // ??
})

type Data = z.infer<typeof schema>

export default function PageSomethingNew() {
  const navigate = useNavigate()

  const {
    handleSubmit,
    control,
    formState: { errors },
  } = useForm<Data>({
    values: {
      name: '',
      number: '',
      integer: '',
      dateTime: '',
      file: '',
    },
    resolver: zodResolver(schema),
  })

  const onSubmit: SubmitHandler<Data> = async data => {
    await apiClient.post('/User/Register', data)
    navigate('/login')
  }

  return (
    <PageAuthorized
      breadcrumbs={breadcrumbs}
      title={title}
    >
      <div className="w-4/5 m-auto flex items-center h-full">
        <Form
          className="grow"
          labelCol={{ span: 3, offset: 5 }}
          wrapperCol={{ span: 8 }}
          layout="horizontal"
          onFinish={handleSubmit(onSubmit)}
        >
          <Controller
            name="name"
            control={control}
            render={({ field }) => (
              <Form.Item
                required
                label="Имя"
                help={errors.name?.message}
                validateStatus={errors.name?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Quaerat magnam
                          dolore possimus repellat. Numquam sapiente, modi voluptas nihil explicabo
                          cum nulla enim sed fugiat, provident ducimus doloremque cumque praesentium
                          officiis.
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input
                      placeholder="Введите имя"
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />
          <Controller
            name="number"
            control={control}
            render={({ field }) => (
              <Form.Item
                required
                label="Десятичная дробь"
                help={errors.number?.message}
                validateStatus={errors.number?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Quaerat magnam
                          dolore possimus repellat. Numquam sapiente, modi voluptas nihil explicabo
                          cum nulla enim sed fugiat, provident ducimus doloremque cumque praesentium
                          officiis.
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input
                      placeholder="Введите значение"
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />
          <Controller
            name="integer"
            control={control}
            render={({ field }) => (
              <Form.Item
                required
                label="Целое число"
                help={errors.integer?.message}
                validateStatus={errors.integer?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Quaerat magnam
                          dolore possimus repellat. Numquam sapiente, modi voluptas nihil explicabo
                          cum nulla enim sed fugiat, provident ducimus doloremque cumque praesentium
                          officiis.
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input
                      placeholder="Введите значение"
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />

          <Form.Item
            label=" "
            colon={false}
          >
            <Button
              type="primary"
              htmlType="submit"
              block
            >
              Создать
            </Button>
          </Form.Item>
        </Form>
      </div>
    </PageAuthorized>
  )
}

// public record CreateSomethingRequest
// {
//     public string Name { get; set; }

//     public double Number { get; set; }

//     public int Integer { get; set; }

//     public DateTime DateTime { get; set; }

//     public IFormFile File { get; set; }
// }
